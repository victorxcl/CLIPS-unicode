//
//  clips.cpp
//  ThinkerDraw-macOS
//
//  Created by 熊 春雷 on 2020/4/24.
//  Copyright © 2020 pandaxcl. All rights reserved.
//

#include "clips.h"
#include "clips.hpp"

void UserFunctions(Environment *environment)
{
#if CLIPS_EXTENSION_TEST_BENCH_ENABLED
    clips::extension::test_benchmark_initialize(environment);
#endif//CLIPS_EXTENSION_TEST_BENCH_ENABLED
    
#if CLIPS_EXTENSION_UTILITY_ENABLED
    clips::extension::utility_initialize(environment);
#endif//CLIPS_EXTENSION_UTILITY_ENABLED
    
#if CLIPS_EXTENSION_SOCKET_ENABLED
    clips::extension::socket_initialize(environment);
#endif//CLIPS_EXTENSION_SOCKET_ENABLED
    
#if CLIPS_EXTENSION_ZEROMQ_ENABLED
    clips::extension::zeromq_initialize(environment);
#endif//CLIPS_EXTENSION_ZEROMQ_ENABLED

#if CLIPS_EXTENSION_MUSTACHE_ENABLED
    clips::extension::mustache_initialize(environment);
#endif//CLIPS_EXTENSION_MUSTACHE_ENABLED
    
}

#if CLIPS_EXTENSION_TEST_BENCH_ENABLED

template<char code> std::ostream&
operator<<(std::ostream&os, const std::tuple<std::string, std::integral_constant<char, code>>&x)
{
    return os << std::get<0>(x);
}

#include <boost/core/lightweight_test.hpp>

namespace clips::extension {

void test_benchmark_initialize(Environment*environment)
{
    clips::user_function<__LINE__>(environment, "test-benchmark", test_benchmark);
}

void test_benchmark()
{
    {
        clips::CLIPS CLIPS;
        BOOST_TEST_EQ(1 +2+3, std::any_cast<clips::integer>(CLIPS.eval("(+ 1  2 3)")));
        BOOST_TEST_EQ(1.+2+3, std::any_cast<clips::real   >(CLIPS.eval("(+ 1. 2 3)")));
        
        BOOST_TEST_EQ(clips::string{"2019-10-10"}, std::any_cast<clips::string>(CLIPS.eval("(str-cat 2019 - 10 - 10)")));
        BOOST_TEST_EQ(clips::symbol{"2019-10-10"}, std::any_cast<clips::symbol>(CLIPS.eval("(sym-cat 2019 - 10 - 10)")));
    }
    
    {
        clips::CLIPS CLIPS;
        {
            using namespace std::string_literals;
            //char argumentsCode[128] = {'*', '\0'};
            std::string argumentsCode = "*";
            BOOST_TEST_EQ("*;v"s, (clips::build_arguments_code<void,         void>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;b"s, (clips::build_arguments_code<void,         bool>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;l"s, (clips::build_arguments_code<void,         char>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;l"s, (clips::build_arguments_code<void,        short>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;l"s, (clips::build_arguments_code<void,          int>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;l"s, (clips::build_arguments_code<void,         long>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;l"s, (clips::build_arguments_code<void,    long long>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;d"s, (clips::build_arguments_code<void,        float>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;d"s, (clips::build_arguments_code<void,       double>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;d"s, (clips::build_arguments_code<void,  long double>::apply(argumentsCode, 1)));
            
            BOOST_TEST_EQ("*;sy"s, (clips::build_arguments_code<void,        char*>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;sy"s, (clips::build_arguments_code<void,  const char*>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;e"s, (clips::build_arguments_code<void,        void*>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;e"s, (clips::build_arguments_code<void, std::string*>::apply(argumentsCode, 1)));
            
            BOOST_TEST_EQ("*;b"s, (clips::build_arguments_code<void,       clips::boolean>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;s"s, (clips::build_arguments_code<void,        clips::string>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;y"s, (clips::build_arguments_code<void,        clips::symbol>::apply(argumentsCode, 1)));
            BOOST_TEST_EQ("*;n"s, (clips::build_arguments_code<void, clips::instance_name>::apply(argumentsCode, 1)));
            
#define TEST_ARGUMENTS bool, int, float, const char*, std::string, clips::string, clips::boolean, clips::symbol, clips::instance_name
            clips::user_function<__LINE__>(CLIPS, "test", static_cast<void(*)(TEST_ARGUMENTS)>([](TEST_ARGUMENTS){ }));
            BOOST_TEST_EQ("*;b;l;d;sy;sy;s;b;y;n"s, (clips::build_arguments_code<void, TEST_ARGUMENTS>::apply(argumentsCode, 1)));
#undef  TEST_ARGUMENTS
        }
        
        clips::user_function<__LINE__>(CLIPS, "hello", static_cast<clips::string(*)()>([]{ return clips::string{"hello"}; }));
        clips::user_function<__LINE__>(CLIPS, "world", static_cast<clips::string(*)()>([]{ return clips::string{"world"}; }));
        
        BOOST_TEST_EQ(clips::string{"hello"}, std::any_cast<clips::string>(CLIPS.eval("(hello)")));
        BOOST_TEST_EQ(clips::string{"world"}, std::any_cast<clips::string>(CLIPS.eval("(world)")));
        BOOST_TEST_EQ(clips::string{"helloworld"}, std::any_cast<clips::string>(CLIPS.eval("(str-cat (hello) (world))")));
        
        
        clips::user_function<__LINE__>(CLIPS, "good", static_cast<clips::string(*)(Environment*)>([](Environment*){ return clips::string{"good"}; }));
        clips::user_function<__LINE__>(CLIPS, "luck", static_cast<clips::string(*)(Environment*)>([](Environment*){ return clips::string{"luck"}; }));
        
        BOOST_TEST_EQ(clips::string{"good"}, std::any_cast<clips::string>(CLIPS.eval("(good)")));
        BOOST_TEST_EQ(clips::string{"luck"}, std::any_cast<clips::string>(CLIPS.eval("(luck)")));
        BOOST_TEST_EQ(clips::string{"goodluck"}, std::any_cast<clips::string>(CLIPS.eval("(str-cat (good) (luck))")));
    }
#if CLIPS_EXTENSION_MUSTACHE_ENABLED
    {
        {
            std::string origin = u8R"(
            Hello
            World
            Happy
            )";
            std::string expected =
            "Hello\n"
            "World\n"
            "Happy";
            
            BOOST_TEST_EQ(clips::string{expected}, mustache_trim(origin.c_str()));
        }
        {
            clips::CLIPS CLIPS;
            std::string view{"{{#names}}Hi {{name}}!\n{{/names}}"};
            std::string context {
                u8R"=========(
                {
                    "names":[
                        { "name": "Chris" },
                        { "name": "Mark" },
                        { "name": "Scott" }
                    ]
                }
                )========="
            };
            
            const char*expected =
            "Hi Chris!\n"
            "Hi Mark!\n"
            "Hi Scott!\n";
            BOOST_TEST_EQ(clips::string{expected}, mustache_render(CLIPS, view.c_str(), context.c_str()));
        }
        {
            clips::CLIPS CLIPS;
            std::string view{"{{#names}}Hi {{> user}}!\n{{/names}}"};
            std::string partials{
                u8R"=========(
                {
                    "user":"<strong>{{name}}</strong>"
                }
                )========="
            };
            std::string context {
                u8R"=========(
                {
                    "names":[
                        { "name": "Chris" },
                        { "name": "Mark" },
                        { "name": "Scott" }
                    ]
                }
                )========="
            };
            
            const char*expected =
            "Hi <strong>Chris</strong>!\n"
            "Hi <strong>Mark</strong>!\n"
            "Hi <strong>Scott</strong>!\n"
            ;
            BOOST_TEST_EQ(clips::string{expected}, mustache_render_with_partials(CLIPS, view.c_str(), context.c_str(), partials.c_str()));
        }
        
    }
#endif//CLIPS_EXTENSION_MUSTACHE_ENABLED
    boost::report_errors();
}

} // clips::extension

#endif//CLIPS_EXTENSION_TEST_BENCH_ENABLED

#if CLIPS_EXTENSION_UTILITY_ENABLED
#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <nlohmann/json.hpp>
#include <future>

namespace clips::extension {

clips::string utility_read_clips(Environment*environment, const char*logicalName)
{
    std::string clips;
    while(!CompleteCommand(clips.c_str())) {
        clips += ReadRouter(environment, logicalName);
    }
    return clips::string{clips};
}

clips::string utility_read_json(Environment*environment, const char*logicalName)
{
    std::string json;
    while(!nlohmann::json::accept(json)) {
        json += ReadRouter(environment, logicalName);
    }
    return clips::string{json};
}

clips::string utility_read_system(Environment*environment, const char* shellCommand)
{
    std::future<std::string> data;
    
    boost::asio::io_context io_context;
    
    boost::process::child c(std::string{shellCommand},
                            //boost::process::std_in.close(),
                            boost::process::std_out > data,
                            //boost::process::std_err > boost::process::null,
                            io_context);
    
    io_context.run(); //this will actually block until the compiler is finished
    
    return clips::string{data.get()};
}

void utility_initialize(Environment*environment)
{
    clips::user_function<__LINE__>(environment, "read-clips",  utility_read_clips);
    clips::user_function<__LINE__>(environment, "read-json",   utility_read_json);
    clips::user_function<__LINE__>(environment, "read-system", utility_read_system);
}

}// namespace clips::extension {
#endif//CLIPS_EXTENSION_UTILITY_ENABLED

#if CLIPS_EXTENSION_SOCKET_ENABLED
#include <boost/asio.hpp>
#include <nlohmann/json.hpp>
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
        enum class Protocol { CLIPS, JSON } protocol {Protocol::CLIPS};
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
            
            if (0 == session->buffer.size()) {
                /*  */ if (socketData::Session::Protocol::CLIPS == session->protocol) {
                    auto rule = [session](auto&&err, std::size_t bytes_transferred)->bool{
                        auto buffer = boost::asio::buffer_cast<const char*>(session->buffer.data());
                        return !!err || CompleteCommand(buffer);
                    };
                    boost::system::error_code ignored_error;
                    boost::asio::read(*session->socket, session->buffer, rule, ignored_error);
                } else if (socketData::Session::Protocol::JSON  == session->protocol) {
                    auto rule = [session](auto&&err, std::size_t bytes_transferred)->bool{
                        auto buffer = boost::asio::buffer_cast<const char*>(session->buffer.data());
                        return !!err || nlohmann::json::accept(buffer);
                    };
                    boost::system::error_code ignored_error;
                    boost::asio::read(*session->socket, session->buffer, rule, ignored_error);
                }
            }
            
            std::istream is(&session->buffer);
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

    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
}

void socket_connect(Environment*environment, const char*ROUTER, const char* HOST, const char* PORT)
{
    if (nullptr == SocketData(environment)->acceptor) {
        SocketData(environment)->resolver = std::make_shared<tcp::resolver>(SocketData(environment)->io_context);
    }

    try {
        auto&io_context = SocketData(environment)->io_context;
        auto&  resolver = SocketData(environment)->resolver;
        
        tcp::resolver::results_type endpoints = resolver->resolve(HOST, PORT);

        auto socket = std::make_shared<tcp::socket>(io_context);
        boost::asio::connect(*socket, endpoints);
        
        _socket_make_session(environment, socket, ROUTER);

    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
}

clips::symbol socket_protocol(Environment*environment, const char*ROUTER, const char*PROTOCOL)
{
    const char*protocol_last = "unknown";
    try {
        auto session = SocketData(environment)->session_list.at(ROUTER);
        
        /*  */ if (socketData::Session::Protocol::CLIPS == session->protocol) {
            protocol_last = "clips";
        } else if (socketData::Session::Protocol::JSON  == session->protocol) {
            protocol_last = "json";
        }
        
        /*  */ if (0 == std::strcmp(PROTOCOL, "clips")) {
            session->protocol = socketData::Session::Protocol::CLIPS;
        } else if (0 == std::strcmp(PROTOCOL, "json")) {
            session->protocol = socketData::Session::Protocol::JSON;
        } else {
            throw std::invalid_argument("\nERROR:\n\t socket protocol only support: clips, json\n");
        }
        
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::symbol{ protocol_last };
}

void socket_initialize(Environment*environment)
{
    if (nullptr == SocketData(environment)) {
        AllocateEnvironmentData(environment, SOCKET_DATA, sizeof(socketData), [](Environment*environment){
            SocketData(environment)->~socketData();
        });
        new (SocketData(environment)) socketData();
    }

    clips::user_function<__LINE__>(environment, "socket-accept",   socket_accept);
    clips::user_function<__LINE__>(environment, "socket-connect",  socket_connect);
    clips::user_function<__LINE__>(environment, "socket-protocol", socket_protocol);
}

}// namespace clips::extension {

#endif// CLIPS_EXTENSION_SOCKET_ENABLED

#if CLIPS_EXTENSION_ZEROMQ_ENABLED

#include <zmq.hpp>
#include <iostream>
#include <unordered_map>
#include <boost/asio/streambuf.hpp>

namespace clips::extension {

struct zeromqData {
    zmq::context_t context;
    struct Session {
        std::string                     router;
        std::shared_ptr<zmq::socket_t>  socket;
        boost::asio::streambuf          buffer_recv;
        boost::asio::streambuf          buffer_send;
        enum class Protocol {
            CLIPS,
            JSON
        } protocol { Protocol::CLIPS };
    };
    std::unordered_map<std::string, std::shared_ptr<Session>>   session_list;
};

#define ZEROMQ_DATA                USER_ENVIRONMENT_DATA + 2
#define ZeromqData(environment)    static_cast<zeromqData*>(GetEnvironmentData(environment, ZEROMQ_DATA))

void _zeromq_make_session(Environment*environment, std::shared_ptr<zmq::socket_t>socket, const char*ROUTER)
{
    auto session = std::make_shared<zeromqData::Session>();
    session->router = ROUTER;
    session->socket = socket;
    ZeromqData(environment)->session_list[ROUTER] = session;
    
    /* prepare router for network */{
        auto RouterQueryFunction = [](Environment *environment, const char *logicalName, void *context) {
            auto session = static_cast<zeromqData::Session*>(context);
            return logicalName == session->router;
        };
        auto RouterWriteFunction = [](Environment *environment, const char *logicalName, const char *str, void *context){
            auto session = static_cast<zeromqData::Session*>(context);
            try {
                std::ostream os(&session->buffer_send);
                os.write(str, std::strlen(str));
                if (auto command=boost::asio::buffer_cast<const char*>(session->buffer_send.data()); CompleteCommand(command)) {
                    session->socket->send(zmq::const_buffer(command, std::strlen(command)), zmq::send_flags::dontwait);
                }
                DeactivateRouter(environment, session->router.c_str());
                WriteString(environment, STDOUT, str);
                ActivateRouter(environment, session->router.c_str());
                
            } catch (std::exception&e) {
                Writeln(environment, e.what());
            }
        };
        auto RouterReadFunction = [](Environment *environment,const char *logicalName,void *context)->int {
            auto session = static_cast<zeromqData::Session*>(context);
            try {
                
                if (0 == session->buffer_recv.size()) {
                    zmq::message_t message;
                    if (auto n = session->socket->recv(message); *n >0 ) {
                        std::ostream os(&session->buffer_recv);
                        os.write(static_cast<char*>(message.data()), *n);
                    }
                }
                
            } catch (std::exception&e) {
                Writeln(environment, e.what());
            }
            std::istream is(&session->buffer_recv);
            return is.get();
        };
        auto RouterUnreadFunction =[](Environment *environment,const char *logicalName,int inchar,void *context)->int {
            auto session = static_cast<zeromqData::Session*>(context);

            std::istream is(&session->buffer_recv);
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

zmq::socket_type _zeromq_socket_type_from_string(const char*SOCKET_TYPE)
{
    zmq::socket_type socket_type;
    
    /* */if (0 == std::strcmp(SOCKET_TYPE, "req"   )) return zmq::socket_type::req;
    else if (0 == std::strcmp(SOCKET_TYPE, "rep"   )) return zmq::socket_type::rep;
    else if (0 == std::strcmp(SOCKET_TYPE, "dealer")) return zmq::socket_type::dealer;
    else if (0 == std::strcmp(SOCKET_TYPE, "router")) return zmq::socket_type::router;
    else if (0 == std::strcmp(SOCKET_TYPE, "pub"   )) return zmq::socket_type::pub;
    else if (0 == std::strcmp(SOCKET_TYPE, "sub"   )) return zmq::socket_type::sub;
    else if (0 == std::strcmp(SOCKET_TYPE, "xpub"  )) return zmq::socket_type::xpub;
    else if (0 == std::strcmp(SOCKET_TYPE, "xsub"  )) return zmq::socket_type::xsub;
    else if (0 == std::strcmp(SOCKET_TYPE, "push"  )) return zmq::socket_type::push;
    else if (0 == std::strcmp(SOCKET_TYPE, "pull"  )) return zmq::socket_type::pull;
        
#if defined(ZMQ_BUILD_DRAFT_API) && ZMQ_VERSION >= ZMQ_MAKE_VERSION(4, 2, 0)
    else if (0 == std::strcmp(SOCKET_TYPE, "server")) return zmq::socket_type::server;
    else if (0 == std::strcmp(SOCKET_TYPE, "client")) return zmq::socket_type::client;
    else if (0 == std::strcmp(SOCKET_TYPE, "radio" )) return zmq::socket_type::radio;
    else if (0 == std::strcmp(SOCKET_TYPE, "dish"  )) return zmq::socket_type::dish;
#endif
    
#if ZMQ_VERSION_MAJOR >= 4
    else if (0 == std::strcmp(SOCKET_TYPE, "stream")) return zmq::socket_type::stream;
#endif
    
    else if (0 == std::strcmp(SOCKET_TYPE, "pair")) return zmq::socket_type::pull;
    
    else {
        throw std::invalid_argument("\nERROR:\tzeromq socket type only support: \n\n"
                                    "\treq, rep, dealer, router, pub, sub, xpub, xsub, push, pull,\n"
                                    "\tserver, client, radio, dish, stream, pair\n");
    }
    
    return socket_type;
}
void zeromq_bind(Environment*environment, const char* ROUTER, const char* ADDRESS, const char* SOCKET_TYPE)
{
    try {
        
        zmq::socket_type socket_type = _zeromq_socket_type_from_string(SOCKET_TYPE);
        auto socket = std::make_shared<zmq::socket_t>(ZeromqData(environment)->context, socket_type);
        
        _zeromq_make_session(environment, socket, ROUTER);
        
        socket->bind(ADDRESS);
        
    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
}

void zeromq_connect(Environment*environment, const char* ROUTER, const char* ADDRESS, const char* SOCKET_TYPE)
{
    try {
        
        zmq::socket_type socket_type = _zeromq_socket_type_from_string(SOCKET_TYPE);
        auto socket = std::make_shared<zmq::socket_t>(ZeromqData(environment)->context, socket_type);
        
        _zeromq_make_session(environment, socket, ROUTER);
        
        socket->connect(ADDRESS);
        
    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
}

clips::symbol zeromq_protocol(Environment*environment, const char*ROUTER, const char*PROTOCOL)
{
    const char*protocol_last = "unknown";
    try {
        auto session = ZeromqData(environment)->session_list.at(ROUTER);
        
        /*  */ if (zeromqData::Session::Protocol::CLIPS == session->protocol) {
            protocol_last = "clips";
        } else if (zeromqData::Session::Protocol::JSON  == session->protocol) {
            protocol_last = "json";
        }
        
        /*  */ if (0 == std::strcmp(PROTOCOL, "clips")) {
            session->protocol = zeromqData::Session::Protocol::CLIPS;
        } else if (0 == std::strcmp(PROTOCOL, "json")) {
            session->protocol = zeromqData::Session::Protocol::JSON;
        } else {
            throw std::invalid_argument("\nERROR:\n\t zmq protocol only support: clips, json\n");
        }
        
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::symbol{ protocol_last };
}

void zeromq_initialize(Environment*environment)
{
    if (nullptr == ZeromqData(environment)) {
        AllocateEnvironmentData(environment, ZEROMQ_DATA, sizeof(zeromqData), [](Environment*environment){
            ZeromqData(environment)->~zeromqData();
        });
        new (ZeromqData(environment)) zeromqData();
    }
    
    clips::user_function<__LINE__>(environment, "zmq-bind",     zeromq_bind);
    clips::user_function<__LINE__>(environment, "zmq-connect",  zeromq_connect);
    clips::user_function<__LINE__>(environment, "zmq-protocol", zeromq_protocol);
}

}// namespace clips::extension {
#endif// CLIPS_EXTENSION_ZEROMQ_ENABLED

#if CLIPS_EXTENSION_MUSTACHE_ENABLED

#include <mstch/mstch.hpp>
#include <nlohmann/json.hpp>
#include <boost/algorithm/string.hpp>

namespace clips::extension {

mstch::node mstch_node_from_json(const nlohmann::json&json)
{
    /**/   if (json.is_null()) {
        return nullptr;
    } else if (json.is_number_float()) {
        return static_cast<double>(json.get<nlohmann::json::number_float_t>());
    } else if (json.is_number_integer()) {
        return static_cast<int>(json.get<nlohmann::json::number_integer_t>());
    } else if (json.is_number_unsigned()) {
        return static_cast<int>(json.get<nlohmann::json::number_unsigned_t>());
    } else if (json.is_boolean()) {
        return static_cast<bool>(json.get<nlohmann::json::boolean_t>());
    } else if (json.is_string()) {
        return json.get<nlohmann::json::string_t>();
    } else if (json.is_object()) {
        mstch::map map;
        for (auto& [key, val] : json.items()) {
            map[key] = mstch_node_from_json(val);
        }
        return map;
    } else if (json.is_array()) {
        mstch::array array;
        for (auto& [key, val] : json.items()) {
            array.push_back(mstch_node_from_json(val));
        }
        return array;
    }
    
    return nullptr;
}

void trim_prefix(std::string&Input)
{
    if (Input.empty())
        return;
    
    std::vector<std::string> v; // #2: Search for tokens
    boost::trim_right(Input);
    boost::split(v, Input, boost::is_any_of("\n"), boost::token_compress_on);
    
    if (v.empty())
        return;
    if (1 == v.size())
        return;
    
    v.erase(std::remove_if(std::begin(v), std::end(v), [](auto x){ return x.empty(); }), std::end(v));
    if (v.size() >= 2) {
        auto n = std::size(v[0]) - std::size(boost::trim_left_copy(v[0]));
        std::for_each(std::begin(v), std::end(v), [n](auto&&x){ boost::erase_head(x, static_cast<int>(n)); });
    }
    Input = boost::join(v, "\n");
}

clips::string mustache_trim(const char* input)
{
    std::string tmp(input);
    trim_prefix(tmp);
    return clips::string{tmp};
}

clips::string mustache_render(Environment*environment, const char* VIEW, const char* CONTEXT)
{
    try {
        mstch::node context = mstch_node_from_json(nlohmann::json::parse(CONTEXT));
        return clips::string{mstch::render(VIEW, context)};
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::string{""};
}

clips::string mustache_render_with_partials(Environment*environment, const char* VIEW, const char* CONTEXT, const char*PARTIALS)
{
    try {
        mstch::node context  = mstch_node_from_json(nlohmann::json::parse(CONTEXT));
        mstch::node partials = mstch_node_from_json(nlohmann::json::parse(PARTIALS));
        std::map<std::string,std::string> partials_map;
        
        for (auto&&[key, node]:boost::get<mstch::map>(partials)) {
            partials_map[key] = boost::get<std::string>(node);
        }
        return clips::string{mstch::render(VIEW, context, partials_map)};
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::string{""};
}

void mustache_initialize(Environment*environment)
{
    clips::user_function<__LINE__>(environment, "mustache-trim",    mustache_trim);
    clips::user_function<__LINE__>(environment, "mustache-render",  mustache_render);
    clips::user_function<__LINE__>(environment, "mustache-render-with-partials",  mustache_render_with_partials);
}

}// namespace clips::extension {
#endif// CLIPS_EXTENSION_MUSTACHE_ENABLED
