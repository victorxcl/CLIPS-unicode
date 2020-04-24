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
#if CLIPS_EXTENSION_NETWORK_ENABLED
    clips::extension::network_initialize(env);
#endif//CLIPS_EXTENSION_NETWORK_ENABLED
}

#if CLIPS_EXTENSION_NETWORK_ENABLED
#include <boost/asio.hpp>
#include <iostream>
#include <unordered_map>
#include <array>

namespace clips::extension {
using boost::asio::ip::tcp;
struct networkData {
    boost::asio::io_context         io_context;
    std::shared_ptr<tcp::acceptor>  acceptor;
    std::shared_ptr<tcp::resolver>  resolver;
    struct Session {
        std::string                     router;
        std::shared_ptr<tcp::socket>    socket;
    };
    std::unordered_map<std::string, std::shared_ptr<Session>>   session_list;
};

#define NETWORK_DATA                USER_ENVIRONMENT_DATA + 1
#define NetworkData(environment)    static_cast<networkData*>(GetEnvironmentData(environment, NETWORK_DATA))

void _network_make_session(Environment*environment, std::shared_ptr<tcp::socket>socket, const char*ROUTER)
{
    auto session = std::make_shared<networkData::Session>();
    session->router = ROUTER;
    session->socket = socket;
    
    /* prepare router for network */{
        auto RouterQueryFunction = [](Environment *environment, const char *logicalName, void *context) {
            auto session = static_cast<networkData::Session*>(context);
            return logicalName == session->router;
        };
        auto RouterWriteFunction = [](Environment *environment, const char *logicalName, const char *str, void *context){
            auto session = static_cast<networkData::Session*>(context);
            boost::system::error_code ignored_error;
            boost::asio::write(*session->socket,
                               boost::asio::buffer(std::string(str)),
                               ignored_error);
            DeactivateRouter(environment, session->router.c_str());
            WriteString(environment, STDOUT, str);
            ActivateRouter(environment, session->router.c_str());
        };
        auto RouterReadFunction = [](Environment *environment,const char *logicalName,void *context)->int {
            auto socket_info = static_cast<networkData::Session*>(context);
            char buffer[1];
            boost::system::error_code ignored_error;
            boost::asio::read(*socket_info->socket,
                              boost::asio::buffer(buffer),
                              boost::asio::transfer_exactly(1),
                              ignored_error);
            return buffer[0];
        };
        AddRouter(environment, ROUTER, 40,
                  /* RouterQueryFunction  * */RouterQueryFunction,
                  /* RouterWriteFunction  * */RouterWriteFunction,
                  /* RouterReadFunction   * */RouterReadFunction,
                  /* RouterUnreadFunction * */nullptr,
                  /* RouterExitFunction   * */nullptr,
                  /* void                 * */session.get());
    }
}

void network_accept(Environment*environment, const char*ROUTER, const int PORT)
{
    if (nullptr == NetworkData(environment)->acceptor) {
        NetworkData(environment)->acceptor = std::make_shared<tcp::acceptor>(NetworkData(environment)->io_context,
                                                                             tcp::endpoint(tcp::v4(), PORT));
    }
    
    try {
        auto&io_context = NetworkData(environment)->io_context;
        auto acceptor = NetworkData(environment)->acceptor;
        
        auto socket = std::make_shared<tcp::socket>(io_context);
        acceptor->accept(*socket);
        
        _network_make_session(environment, socket, ROUTER);
        
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

void network_connect(Environment*environment, const char*ROUTER, const char* HOST, const char* PORT)
{
    if (nullptr == NetworkData(environment)->acceptor) {
        NetworkData(environment)->resolver = std::make_shared<tcp::resolver>(NetworkData(environment)->io_context);
    }
    
    try {
        auto&io_context = NetworkData(environment)->io_context;
        
        auto&resolver = NetworkData(environment)->resolver;
        tcp::resolver::results_type endpoints = resolver->resolve(HOST, PORT);
        
        auto socket = std::make_shared<tcp::socket>(io_context);
        boost::asio::connect(*socket, endpoints);
        
        _network_make_session(environment, socket, ROUTER);
        
    } catch (std::exception& e) {
        std::cerr << e.what() << std::endl;
    }
}

void network_initialize(Environment*environment)
{
    if (nullptr == NetworkData(environment)) {
        AllocateEnvironmentData(environment, NETWORK_DATA, sizeof(networkData), [](Environment*environment){
            NetworkData(environment)->~networkData();
        });
        new (NetworkData(environment)) networkData();
    }
    
    clips::user_function<__LINE__>(environment, "network_accept",  network_accept);
    clips::user_function<__LINE__>(environment, "network_connect", network_connect);
}

}// namespace clips::extension {

#endif// CLIPS_EXTENSION_NETWORK_ENABLED
