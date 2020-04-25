//
//  clips.cpp
//  ThinkerDraw-macOS
//
//  Created by 熊 春雷 on 2020/4/24.
//  Copyright © 2020 pandaxcl. All rights reserved.
//

#include "clips.h"
#include "clips.hpp"

void UserFunctions(Environment *env)
{
#if CLIPS_EXTENSION_SOCKET_ENABLED
    clips::extension::socket_initialize(env);
#endif//CLIPS_EXTENSION_SOCKET_ENABLED
    
}

#if CLIPS_EXTENSION_SOCKET_ENABLED && 1
#include <boost/asio.hpp>
#include <iostream>
#include <unordered_map>
#include <array>

namespace clips::extension {
using boost::asio::ip::tcp;
struct socketData {
    boost::asio::io_context         io_context;
    std::shared_ptr<tcp::acceptor>  acceptor;
    std::shared_ptr<tcp::resolver>  resolver;
    struct Session {
        std::string                     router;
        std::shared_ptr<tcp::socket>    socket;
        boost::asio::streambuf          buffer;
    };
    std::unordered_map<std::string, std::shared_ptr<Session>>   session_list;
};

#define SOCKET_DATA                USER_ENVIRONMENT_DATA + 1
#define SocketData(environment)    static_cast<socketData*>(GetEnvironmentData(environment, SOCKET_DATA))

void _socket_make_session(Environment*environment, std::shared_ptr<tcp::socket>socket, const char*ROUTER)
{
    auto session = std::make_shared<socketData::Session>();
    session->router = ROUTER;
    session->socket = socket;
    SocketData(environment)->session_list[ROUTER] = session;

    /* prepare router for network */{
        auto RouterQueryFunction = [](Environment *environment, const char *logicalName, void *context) {
            auto session = static_cast<socketData::Session*>(context);
            return logicalName == session->router;
        };
        auto RouterWriteFunction = [](Environment *environment, const char *logicalName, const char *str, void *context){
            auto session = static_cast<socketData::Session*>(context);
            
            boost::system::error_code ignored_error;
            boost::asio::write(*session->socket, boost::asio::buffer(std::string(str)), ignored_error);
            
            DeactivateRouter(environment, session->router.c_str());
            WriteString(environment, STDOUT, str);
            ActivateRouter(environment, session->router.c_str());
        };
        auto RouterReadFunction = [](Environment *environment,const char *logicalName,void *context)->int {
            auto session = static_cast<socketData::Session*>(context);
        
            auto&buffer = session->buffer;
            
            if (0 == buffer.size()) {
                boost::system::error_code ignored_error;
                boost::asio::read_until(*session->socket, buffer, '\n', ignored_error);
            }
            
            std::istream is(&buffer);
            return is.get();
        };
        auto RouterUnreadFunction =[](Environment *environment,const char *logicalName,int inchar,void *context)->int {
            auto session = static_cast<socketData::Session*>(context);
            
            auto&buffer = session->buffer;
            std::istream is(&buffer);
            is.putback(inchar);
            
            return static_cast<int>(true);
        };
        
        AddRouter(environment, ROUTER, 40,
                  /* RouterQueryFunction  * */RouterQueryFunction,
                  /* RouterWriteFunction  * */RouterWriteFunction,
                  /* RouterReadFunction   * */RouterReadFunction,
                  /* RouterUnreadFunction * */RouterUnreadFunction,
                  /* RouterExitFunction   * */nullptr,
                  /* void                 * */session.get());
    }
}

void socket_accept(Environment*environment, const char*ROUTER, const int PORT)
{
    if (nullptr == SocketData(environment)->acceptor) {
        SocketData(environment)->acceptor = std::make_shared<tcp::acceptor>(SocketData(environment)->io_context,
                                                                             tcp::endpoint(tcp::v4(), PORT));
    }

    try {
        auto&io_context = SocketData(environment)->io_context;
        auto acceptor = SocketData(environment)->acceptor;

        auto socket = std::make_shared<tcp::socket>(io_context);
        acceptor->accept(*socket);

        _socket_make_session(environment, socket, ROUTER);

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

void socket_connect(Environment*environment, const char*ROUTER, const char* HOST, const char* PORT)
{
    if (nullptr == SocketData(environment)->acceptor) {
        SocketData(environment)->resolver = std::make_shared<tcp::resolver>(SocketData(environment)->io_context);
    }

    try {
        auto&io_context = SocketData(environment)->io_context;

        auto&resolver = SocketData(environment)->resolver;
        tcp::resolver::results_type endpoints = resolver->resolve(HOST, PORT);

        auto socket = std::make_shared<tcp::socket>(io_context);
        boost::asio::connect(*socket, endpoints);

        _socket_make_session(environment, socket, ROUTER);

    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

void socket_initialize(Environment*environment)
{
    if (nullptr == SocketData(environment)) {
        AllocateEnvironmentData(environment, SOCKET_DATA, sizeof(socketData), [](Environment*environment){
            SocketData(environment)->~socketData();
        });
        new (SocketData(environment)) socketData();
    }

    clips::user_function<__LINE__>(environment, "socket-accept",  socket_accept);
    clips::user_function<__LINE__>(environment, "socket-connect", socket_connect);
}

}// namespace clips::extension {

#endif// CLIPS_EXTENSION_SOCKET_ENABLED
