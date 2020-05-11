//
//  clips.cpp
//  ThinkerDraw-macOS
//
//  Created by 熊 春雷 on 2020/4/24.
//  Copyright © 2020 pandaxcl. All rights reserved.
//

#include "clips.h"
#include "clips.hpp"

#ifdef system // defined in usrsetup.h for iOS
#   undef system // but here use boost::process::system instead
#endif

#ifdef LHS // conflict with uuidgen
#   undef LHS
#endif

#ifdef RHS // conflict with uuidgen
#   undef RHS
#endif

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
    
#if CLIPS_EXTENSION_PROCESS_ENABLED
    clips::extension::process_initialize(environment);
#endif//CLIPS_EXTENSION_PROCESS_ENABLED
    
}

#if CLIPS_EXTENSION_TEST_BENCH_ENABLED

template<char code> std::ostream&
operator<<(std::ostream&os, const std::tuple<std::string, std::integral_constant<char, code>>&x)
{
    return os << std::get<0>(x);
}

#include <boost/core/lightweight_test.hpp>
#include <boost/format.hpp>
#include <nlohmann/json.hpp>

namespace clips::extension {

void test_benchmark_initialize(Environment*environment)
{
    clips::user_function<__LINE__>(environment, "test-benchmark", test_benchmark);
}

void test_benchmark()
{
    {
        clips::CLIPS CLIPS;
        BOOST_TEST_NOT(CompleteCommand("?A"));
        BOOST_TEST_NOT(CompleteCommand("\n\n\n?A"));
        BOOST_TEST(    CompleteCommand(      "?A\n\n\n"));
        BOOST_TEST(    CompleteCommand("\n\n\n?A\n\n\n"));
        
        BOOST_TEST_NOT(CompleteCommand("(expand$ ?A)"));
        BOOST_TEST_NOT(CompleteCommand("\n\n\n(expand$ ?A)"));
        BOOST_TEST(    CompleteCommand(      "(expand$ ?A)\n\n\n"));
        BOOST_TEST(    CompleteCommand("\n\n\n(expand$ ?A)\n\n\n"));
    }
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
#if CLIPS_EXTENSION_UTILITY_ENABLED
    {
        {
            clips::CLIPS CLIPS;
            void*json = std::any_cast<void*>(CLIPS.eval(u8R"===( (bind ?json (json-create `{
                                                        "a":1,
                                                        "b":"hello",
                                                        "c":12.5,
                                                        "baz": ["one", "two", "three"],
                                                        "foo": "bar"
                                                        }`)) )==="));
            BOOST_TEST(nullptr != json);
            
            BOOST_TEST_EQ(clips::integer{1      }, std::any_cast<clips::integer>(CLIPS.eval(u8R"===((json-value-for-path ?json /a)    )===")));
            BOOST_TEST_EQ(clips::string {"hello"}, std::any_cast<clips::string >(CLIPS.eval(u8R"===((json-value-for-path ?json /b)    )===")));
            BOOST_TEST_EQ(clips::real   {12.5   }, std::any_cast<clips::real   >(CLIPS.eval(u8R"===((json-value-for-path ?json /c)    )===")));
            BOOST_TEST_EQ(clips::string {"one"  }, std::any_cast<clips::string >(CLIPS.eval(u8R"===((json-value-for-path ?json /baz/0))===")));
            BOOST_TEST_EQ(clips::string {"two"  }, std::any_cast<clips::string >(CLIPS.eval(u8R"===((json-value-for-path ?json /baz/1))===")));
            BOOST_TEST_EQ(clips::string {"three"}, std::any_cast<clips::string >(CLIPS.eval(u8R"===((json-value-for-path ?json /baz/2))===")));
            BOOST_TEST_EQ(clips::string {"bar"  }, std::any_cast<clips::string >(CLIPS.eval(u8R"===((json-value-for-path ?json /foo)  )===")));
            
            CLIPS.eval("(json-set-value-for-path ?json /baz/0 100)");
            BOOST_TEST_EQ(clips::integer{100}, std::any_cast<clips::integer>(CLIPS.eval(u8R"===((json-value-for-path ?json /baz/0))===")));
            
            CLIPS.eval(R"===( (json-set-value-for-path ?json /baz/1 `{"hello":200}`) )===");
            BOOST_TEST_EQ(clips::integer{200}, std::any_cast<clips::integer>(CLIPS.eval(u8R"===((json-value-for-path ?json /baz/1/hello))===")));
            
            CLIPS.eval("(json-dispose ?json)");
        }
        {
            clips::CLIPS CLIPS;
            const std::string original = u8R"===({
            "baz": ["one", "two", "three"],
            "foo": "bar"
            })===";
            const std::string patch = u8R"===([
            { "op": "replace", "path": "/baz", "value": "boo" },
            { "op": "remove", "path": "/foo"},
            { "op": "add", "path": "/hello", "value": ["world"] }
            ])===";
            const std::string result = u8R"===({
            "baz": "boo",
            "hello": ["world"]
            })===";
            CLIPS.eval((boost::format("(bind ?original (json-create `%1%`))")%original).str().c_str());
            CLIPS.eval((boost::format("(bind ?patch    (json-create `%1%`))")%patch   ).str().c_str());
            CLIPS.eval((boost::format("(bind ?result   (json-create `%1%`))")%result  ).str().c_str());
            
            CLIPS.eval("(bind ?original-with-patch  (json-patch ?original ?patch ))");
            CLIPS.eval("(bind ?original-diff-result (json-diff  ?original ?result))");
            
            BOOST_TEST_EQ(nlohmann::json::parse(result), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?original-with-patch  -1)")))));
            BOOST_TEST_EQ(nlohmann::json::parse(patch ), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?original-diff-result -1)")))));
            
            BOOST_TEST_EQ(nlohmann::json::parse(original), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?original -1)")))));
            BOOST_TEST_EQ(nlohmann::json::parse(patch   ), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?patch    -1)")))));
            BOOST_TEST_EQ(nlohmann::json::parse(result  ), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?result   -1)")))));
            
            CLIPS.eval("(json-dispose ?original            )");
            CLIPS.eval("(json-dispose ?patch               )");
            CLIPS.eval("(json-dispose ?result              )");
            CLIPS.eval("(json-dispose ?original-with-patch )");
            CLIPS.eval("(json-dispose ?original-diff-result)");
        }
        {
            clips::CLIPS CLIPS;
            // a JSON value
            const std::string document = R"({
            "a": "b",
            "c": { "d": "e", "f": "g" }
            })";
            
            // a patch
            const std::string patch = R"({
            "a":"z",
            "c": { "f": null }
            })";
            
            // the merge patch result
            const std::string result = R"({
            "a": "z",
            "c": { "d": "e" }
            })";
            
            CLIPS.eval((boost::format("(bind ?document (json-create `%1%`))")%document).str().c_str());
            CLIPS.eval((boost::format("(bind ?patch    (json-create `%1%`))")%patch   ).str().c_str());
            
            CLIPS.eval("(bind ?document-merge-patch  (json-merge-patch ?document ?patch))");
            
            BOOST_TEST_EQ(nlohmann::json::parse(result), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?document-merge-patch -1)")))));
            
            BOOST_TEST_EQ(nlohmann::json::parse(result), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?document -1)")))));
            BOOST_TEST_EQ(nlohmann::json::parse(patch ), nlohmann::json::parse(std::get<0>(std::any_cast<clips::symbol>(CLIPS.eval("(json-dump ?patch    -1)")))));
            
            CLIPS.eval("(json-dispose ?document)");
            CLIPS.eval("(json-dispose ?patch   )");
        }
    }
#endif// CLIPS_EXTENSION_UTILITY_ENABLED
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
#include <nlohmann/json.hpp>
#include <future>
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>

namespace clips::extension {

clips::string utility_read_sexp(Environment*environment, const char*logicalName)
{
    std::string sexp;
    while(!CompleteCommand(sexp.c_str())) {
        int inchar = ReadRouter(environment, logicalName);
        if (EOF == inchar)
            break;
        sexp += inchar;
    }
    return clips::string{sexp};
}

clips::string utility_read_json(Environment*environment, const char*logicalName)
{
    std::string json;
    while(!nlohmann::json::accept(json)) {
        int inchar = ReadRouter(environment, logicalName);
        if (EOF == inchar)
            break;
        json += inchar;
    }
    return clips::string{json};
}

clips::string utility_read_until(Environment*environment, const char*logicalName, const char*MATCH)
{
    std::string buffer;
    while(!boost::ends_with(buffer, MATCH)) {
        int inchar = ReadRouter(environment, logicalName);
        if (EOF == inchar)
            break;
        buffer += inchar;
    }
    return clips::string{buffer.substr(0, buffer.length()-std::strlen(MATCH))};
}

clips::string utility_expand_for_eval(Environment*environment, const char*CODE)
{
    std::string str1(u8R"===({"abc":",?A","def":",(expand$ ?B)"})===");
    
    std::vector<std::string> items;
    
    std::string buffer;
    for (char c: std::string{str1}) {
        buffer += c;
        
        /*  */ if (boost::ends_with(buffer, ",?")) {
            items.push_back(buffer.substr(0, buffer.length()-2));
            buffer = "?";
        } else if (boost::ends_with(buffer, ",(")) {
            items.push_back(buffer.substr(0, buffer.length()-2));
            buffer = "(";
        } else if (boost::starts_with(buffer, "?") && boost::is_any_of("'(`\"\n\t")(c)) {
            items.push_back(" " + buffer.substr(0, buffer.length()-1) + " ");
            buffer = buffer.substr(buffer.length()-1);
        } else if (boost::starts_with(buffer, "(") && CompleteCommand((buffer+"\n").c_str())) {
            items.push_back(" " + buffer + " ");
            buffer.clear();
        }
    }
    if (!buffer.empty()) {
        items.push_back(buffer);
    }
    
    for (auto&&item:items) {
        std::cout << "item: [" << item << "]" << std::endl;
    }
    
    buffer = "(str-cat `" + boost::join(items, "`") + "`)";
    std::cout << buffer << std::endl;
    
    return clips::string{buffer};
}

clips::string utility_expand_and_eval(Environment*environment, const char*CODE)
{
    auto buffer = utility_expand_for_eval(environment, CODE);
    CLIPSValue value;
    EvalError err = Eval(environment, std::get<0>(buffer).c_str(), &value);
    if (EE_NO_ERROR == err) {
        return clips::string{value.lexemeValue->contents};
    }
    return clips::string{""};
}

clips::integer utility_max_integer(Environment*environment)
{
    return std::numeric_limits<clips::integer>::max();
}
clips::integer utility_min_integer(Environment*environment)
{
    return std::numeric_limits<clips::integer>::min();
}

clips::symbol utility_uuidgen(Environment*environment)
{
    boost::uuids::uuid uuid = boost::uuids::random_generator()();
    return clips::symbol{boost::uuids::to_string(uuid)};
}

void utility_sleep_seconds(Environment*environment, clips::integer n)
{
    std::this_thread::sleep_for(std::chrono::seconds(n));
}
void utility_sleep_milliseconds(Environment*environment, clips::integer n)
{
    std::this_thread::sleep_for(std::chrono::milliseconds(n));
}
void utility_sleep_microseconds(Environment*environment, clips::integer n)
{
    std::this_thread::sleep_for(std::chrono::microseconds(n));
}
void utility_sleep_nanoseconds(Environment*environment, clips::integer n)
{
    std::this_thread::sleep_for(std::chrono::nanoseconds(n));
}

clips::external_address utility_json_create(Environment*environment, const char*JSON)
{
    void *pointer = genalloc(environment, sizeof(nlohmann::json));
    new (pointer) nlohmann::json(nlohmann::json::parse(JSON));
    return clips::external_address{pointer};
}

void utility_json_dispose(Environment*environment, void*pointer)
{
    static_cast<nlohmann::json*>(pointer)->~basic_json();
    genfree(environment, pointer, sizeof(nlohmann::json));
}

clips::boolean utility_json_validate(Environment*environment, const char*JSON)
{
    return clips::boolean{nlohmann::json::accept(JSON)};
}
clips::symbol utility_json_dump(Environment*environment, nlohmann::json*json, int indent)
{
    return clips::symbol{static_cast<nlohmann::json*>(json)->dump(indent)};
}
clips::symbol utility_json_dump_dispose(Environment*environment, nlohmann::json*json, int indent)
{
    auto symbol_dump = utility_json_dump(environment, json, indent);
    utility_json_dispose(environment, json);
    return symbol_dump;
}
clips::external_address utility_json_diff(Environment*environment, nlohmann::json*A, nlohmann::json*B)
{
    void *pointer = genalloc(environment, sizeof(nlohmann::json));
    new (pointer) nlohmann::json(nlohmann::json::diff(*A, *B));
    return clips::external_address{pointer};
}
clips::external_address utility_json_patch(Environment*environment, nlohmann::json*A, nlohmann::json*B)
{
    void *pointer = genalloc(environment, sizeof(nlohmann::json));
    new (pointer) nlohmann::json(A->patch(*B));
    return clips::external_address{pointer};
}
clips::external_address utility_json_merge_patch(Environment*environment, nlohmann::json*A, nlohmann::json*B)
{
    A->merge_patch(*B);
    return clips::external_address{A};
}

// void*pointer, const char*KEY, const char*DEFAULT
static void utility_json_value_for_path(Environment*environment, UDFContext *context, UDFValue*out)
{
    UDFValue pointer, key;
    
    UDFFirstArgument(context, clips::argument_code<clips::external_address>::expect_bits, &pointer);
    UDFNextArgument (context,
                     clips::argument_code<clips::string>::expect_bits|
                     clips::argument_code<clips::symbol>::expect_bits, &key);
    if (UDFHasNextArgument(context)) {
        UDFNextArgument (context,
                         clips::argument_code<clips::boolean>::expect_bits|
                         clips::argument_code<clips::integer>::expect_bits|
                         clips::argument_code<clips::real   >::expect_bits|
                         clips::argument_code<clips::string >::expect_bits|
                         clips::argument_code<clips::symbol >::expect_bits, out);
    }
    try {
        nlohmann::json* json = static_cast<nlohmann::json*>(pointer.externalAddressValue->contents);
        nlohmann::json_pointer pointer = nlohmann::json::json_pointer(key.lexemeValue->contents);
        if (json->contains(pointer)) {
            nlohmann::json&value = json->at(pointer);
            
            /*  */ if (value.is_string()) {
                out->lexemeValue = CreateString(environment, value.get<std::string>().c_str());
            } else if (value.is_boolean()) {
                out->lexemeValue = CreateBoolean(environment, value.get<bool>());
            } else if (value.is_number_float()) {
                out->floatValue = CreateFloat(environment, value.get<double>());
            } else if (value.is_number_integer() || value.is_number_unsigned()) {
                out->integerValue = CreateInteger(environment, value.get<long long>());
            } else {
                out->lexemeValue = CreateSymbol(environment, value.dump().c_str());
            }
        }
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
        UDFThrowError(context);
    }
}

static void _AddUDF_utility_json_value_for_path(Environment*environment)
{
    char returnCode[] = {
        clips::return_code<clips::boolean>::value,
        clips::return_code<clips::integer>::value,
        clips::return_code<clips::real   >::value,
        clips::return_code<clips::string >::value,
        clips::return_code<clips::symbol >::value, 0
    };
    char argumentsCode[] = { '*', ';',
        clips::argument_code<clips::external_address>::value, ';',
        
        clips::argument_code<clips::string>::value,
        clips::argument_code<clips::symbol>::value, ';',
        
        clips::argument_code<clips::boolean>::value,
        clips::argument_code<clips::integer>::value,
        clips::argument_code<clips::real   >::value,
        clips::argument_code<clips::string >::value,
        clips::argument_code<clips::symbol >::value, 0
    };
    AddUDF(environment, "json-value-for-path", returnCode, 2, 3, argumentsCode,
           utility_json_value_for_path, "utility_json_value_for_path", nullptr);
}

static void utility_json_set_value_for_path(Environment*environment, UDFContext *context, UDFValue*out)
{
    UDFValue pointer, key, value;
    
    UDFFirstArgument(context, clips::argument_code<clips::external_address>::expect_bits, &pointer);
    UDFNextArgument (context,
                     clips::argument_code<clips::string>::expect_bits|
                     clips::argument_code<clips::symbol>::expect_bits, &key);
    UDFNextArgument (context,
                     clips::argument_code<clips::boolean>::expect_bits|
                     clips::argument_code<clips::integer>::expect_bits|
                     clips::argument_code<clips::real   >::expect_bits|
                     clips::argument_code<clips::string >::expect_bits|
                     clips::argument_code<clips::symbol >::expect_bits|
                     clips::argument_code<clips::external_address>::expect_bits, &value);
    try {
        nlohmann::json* json = static_cast<nlohmann::json*>(pointer.externalAddressValue->contents);
        nlohmann::json_pointer pointer = nlohmann::json::json_pointer(key.lexemeValue->contents);
        nlohmann::json&json_target = json->operator[](pointer);
            
        /*  */ if (STRING_TYPE == value.header->type) {
            if (auto&&tmp = nlohmann::json::parse(value.lexemeValue->contents); tmp.is_object() or tmp.is_array()) {
                json_target = tmp;
            } else {
                json_target = value.lexemeValue->contents;
            }
        } else if (SYMBOL_TYPE == value.header->type) {
            /*  */ if (TrueSymbol(environment) == value.lexemeValue) {
                json_target = true;
            } else if (FalseSymbol(environment) == value.lexemeValue) {
                json_target = false;
            } else {
                if (auto&&tmp = nlohmann::json::parse(value.lexemeValue->contents); tmp.is_object() or tmp.is_array()) {
                    json_target = tmp;
                } else {
                    json_target = value.lexemeValue->contents;
                }
            }
        } else if (FLOAT_TYPE == value.header->type) {
            json_target = value.floatValue->contents;
        } else if (INTEGER_TYPE == value.header->type) {
            json_target = value.integerValue->contents;
        } else if (EXTERNAL_ADDRESS_TYPE == value.header->type){
            nlohmann::json*pointer = static_cast<nlohmann::json*>(value.externalAddressValue->contents);
            json_target = *pointer;
        } else {
            throw std::invalid_argument((boost::format("[ERROR] invalid argument for json key path: %1%")%key.lexemeValue->contents).str());
        }
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
        UDFThrowError(context);
    }
}
static void _AddUDF_utility_json_set_value_for_path(Environment*environment)
{
    char returnCode[] = {
        clips::return_code<clips::boolean>::value,
        clips::return_code<clips::integer>::value,
        clips::return_code<clips::real   >::value,
        clips::return_code<clips::string >::value,
        clips::return_code<clips::symbol >::value, 0
    };
    char argumentsCode[] = { '*', ';',
        clips::argument_code<clips::external_address>::value, ';',
        
        clips::argument_code<clips::string>::value,
        clips::argument_code<clips::symbol>::value, ';',
        
        clips::argument_code<clips::boolean>::value,
        clips::argument_code<clips::integer>::value,
        clips::argument_code<clips::real   >::value,
        clips::argument_code<clips::string >::value,
        clips::argument_code<clips::symbol >::value, 0
    };
    AddUDF(environment, "json-set-value-for-path", returnCode, 3, 3, argumentsCode,
           utility_json_set_value_for_path, "utility_json_set_value_for_path", nullptr);
}

void utility_initialize(Environment*environment)
{
    clips::user_function<__LINE__>(environment, "read-sexp",  utility_read_sexp);
    clips::user_function<__LINE__>(environment, "read-json",  utility_read_json);
    clips::user_function<__LINE__>(environment, "read-until", utility_read_until);
    
    clips::user_function<__LINE__>(environment, "max-integer", utility_max_integer);
    clips::user_function<__LINE__>(environment, "min-integer", utility_min_integer);
    
    clips::user_function<__LINE__>(environment, "uuidgen", utility_uuidgen);
    
    clips::user_function<__LINE__>(environment, "sleep-seconds",      utility_sleep_seconds);
    clips::user_function<__LINE__>(environment, "sleep-milliseconds", utility_sleep_milliseconds);
    clips::user_function<__LINE__>(environment, "sleep-microseconds", utility_sleep_microseconds);
    clips::user_function<__LINE__>(environment, "sleep-nanoseconds",  utility_sleep_nanoseconds);
    
    //clips::user_function<__LINE__>(environment, "expand-for-eval", utility_expand_for_eval);
    //clips::user_function<__LINE__>(environment, "expand-and-eval", utility_expand_and_eval);
    
    
    clips::user_function<__LINE__>(environment, "json-validate",    utility_json_validate);

    clips::user_function<__LINE__>(environment, "json-create",      utility_json_create);
    clips::user_function<__LINE__>(environment, "json-dispose",     utility_json_dispose);
    clips::user_function<__LINE__>(environment, "json-dump-dispose",utility_json_dump_dispose);
    clips::user_function<__LINE__>(environment, "json-dump",        utility_json_dump);
    clips::user_function<__LINE__>(environment, "json-diff",        utility_json_diff);
    clips::user_function<__LINE__>(environment, "json-patch",       utility_json_patch);
    clips::user_function<__LINE__>(environment, "json-merge-patch", utility_json_merge_patch);
    
    _AddUDF_utility_json_value_for_path(environment);
    _AddUDF_utility_json_set_value_for_path(environment);
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
        boost::asio::streambuf          buffer_recv;
        bool                            block_reading{false};
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
            boost::asio::write(*session->socket,
                               boost::asio::const_buffer(str, std::strlen(str)),
                               ignored_error);
        };
        auto RouterReadFunction = [](Environment *environment,const char *logicalName,void *context)->int {
            auto session = static_cast<socketData::Session*>(context);
            
            if (0 == session->buffer_recv.size()) {
                
                std::size_t bytes_available = session->socket->available();
                
                 if (0 == bytes_available && session->block_reading) {
                    bytes_available = 1;
                }
                
                if (bytes_available > 0) {
                    boost::system::error_code ignored_error;
                    boost::asio::read(*session->socket,
                                      session->buffer_recv,
                                      boost::asio::transfer_exactly(bytes_available),
                                      ignored_error);
                }
            }
            
            std::istream is(&session->buffer_recv);
            return is.get();
        };
        auto RouterUnreadFunction =[](Environment *environment,const char *logicalName,int inchar,void *context)->int {
            auto session = static_cast<socketData::Session*>(context);
            
            std::istream is(&session->buffer_recv);
            is.putback(inchar);
            
            return static_cast<int>(true);
        };
        
        AddRouter(environment, ROUTER, 20,
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

clips::string socket_peek_available(Environment*environment, const char*ROUTER)
{
    const char* content = "";
    try{
        auto session = SocketData(environment)->session_list.at(ROUTER);
        std::size_t bytes_available = session->socket->available();
        
        if (bytes_available > 0) {
            boost::system::error_code ignored_error;
            boost::asio::read(*session->socket,
                              session->buffer_recv,
                              boost::asio::transfer_exactly(bytes_available),
                              ignored_error);
        }
        
        content = boost::asio::buffer_cast<const char*>(session->buffer_recv.data());
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::string{content};
}

clips::boolean socket_block_reading(Environment*environment, const char*ROUTER, bool block_reading)
{
    bool last_block_reading{false};
    try{
        auto session = SocketData(environment)->session_list.at(ROUTER);
        last_block_reading = session->block_reading;
        session->block_reading = block_reading;
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::boolean{last_block_reading};
}

void socket_initialize(Environment*environment)
{
    if (nullptr == SocketData(environment)) {
        AllocateEnvironmentData(environment, SOCKET_DATA, sizeof(socketData), [](Environment*environment){
            SocketData(environment)->~socketData();
        });
        new (SocketData(environment)) socketData();
    }

    clips::user_function<__LINE__>(environment, "socket-accept",         socket_accept);
    clips::user_function<__LINE__>(environment, "socket-connect",        socket_connect);
    clips::user_function<__LINE__>(environment, "socket-peek-available", socket_peek_available);
    clips::user_function<__LINE__>(environment, "socket-block-reading",  socket_block_reading);
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
            RAW, JSON, SEXP
        } protocol { Protocol::SEXP };
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
                os << str;
                
                auto command = boost::asio::buffer_cast<const char*>(session->buffer_send.data());
                
                /*  */ if (zeromqData::Session::Protocol::RAW == session->protocol) {
                    session->socket->send(zmq::const_buffer(command, std::strlen(command)), zmq::send_flags::dontwait);
                    session->buffer_send.consume(std::strlen(command));
                } else if (zeromqData::Session::Protocol::SEXP == session->protocol && CompleteCommand(command)) {
                    session->socket->send(zmq::const_buffer(command, std::strlen(command)), zmq::send_flags::dontwait);
                    session->buffer_send.consume(std::strlen(command));
                } else if (zeromqData::Session::Protocol::JSON == session->protocol && nlohmann::json::accept(command)) {
                    session->socket->send(zmq::const_buffer(command, std::strlen(command)), zmq::send_flags::dontwait);
                    session->buffer_send.consume(std::strlen(command));
                }
                
            } catch (const std::exception&e) {
                WriteString(environment, STDERR, e.what());
                WriteString(environment, STDERR, "\n");
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
                
            } catch (const std::exception&e) {
                WriteString(environment, STDERR, e.what());
                WriteString(environment, STDERR, "\n");
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
        
        AddRouter(environment, ROUTER, 20,
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
                                                                                                        
        /*  */ if (zeromqData::Session::Protocol::SEXP == session->protocol) {
            protocol_last = "clips";
        } else if (zeromqData::Session::Protocol::JSON  == session->protocol) {
            protocol_last = "json";
        } else if (zeromqData::Session::Protocol::RAW  == session->protocol) {
            protocol_last = "raw";
        }
                                                                                                        
        /*  */ if (0 == std::strcmp(PROTOCOL, "sexp")) {
            session->protocol = zeromqData::Session::Protocol::SEXP;
        } else if (0 == std::strcmp(PROTOCOL, "json")) {
            session->protocol = zeromqData::Session::Protocol::JSON;
        } else if (0 == std::strcmp(PROTOCOL, "raw")) {
            session->protocol = zeromqData::Session::Protocol::RAW;
        } else {
            throw std::invalid_argument("\nERROR:\n\t zeromq protocol only support: sexp, json, raw\n");
        }
                                                                                                        
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::symbol{ protocol_last };
}

#define ZEROMQ_PEEK_BUFFER(send)                                                            \
/**/clips::string zeromq_peek_##send##_buffer(Environment*environment, const char*ROUTER)   \
/**/{                                                                                       \
/**/    const char* content = "";                                                           \
/**/    try{                                                                                \
/**/        auto session = ZeromqData(environment)->session_list.at(ROUTER);                \
/**/        content = boost::asio::buffer_cast<const char*>(session->buffer_##send.data()); \
/**/    } catch (const std::exception&e) {                                                  \
/**/        WriteString(environment, STDERR, e.what());                                     \
/**/        WriteString(environment, STDERR, "\n");                                         \
/**/    }                                                                                   \
/**/    return clips::string{content};                                                      \
/**/}                                                                                       \

ZEROMQ_PEEK_BUFFER(send)
ZEROMQ_PEEK_BUFFER(recv)

#undef ZEROMQ_PEEK_BUFFER

clips::symbol zeromq_version(Environment*environment)
{
    return clips::symbol{std::to_string(ZMQ_VERSION)};
}

void zeromq_initialize(Environment*environment)
{
    if (nullptr == ZeromqData(environment)) {
        AllocateEnvironmentData(environment, ZEROMQ_DATA, sizeof(zeromqData), [](Environment*environment){
            ZeromqData(environment)->~zeromqData();
        });
        new (ZeromqData(environment)) zeromqData();
    }
    
    clips::user_function<__LINE__>(environment, "zmq-bind",             zeromq_bind);
    clips::user_function<__LINE__>(environment, "zmq-connect",          zeromq_connect);
    clips::user_function<__LINE__>(environment, "zmq-protocol",         zeromq_protocol);
    clips::user_function<__LINE__>(environment, "zmq-peek-send-buffer", zeromq_peek_send_buffer);
    clips::user_function<__LINE__>(environment, "zmq-peek-recv-buffer", zeromq_peek_recv_buffer);
    clips::user_function<__LINE__>(environment, "zmq-version",          zeromq_version);
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


#if CLIPS_EXTENSION_PROCESS_ENABLED

#include <boost/asio.hpp>
#include <boost/process.hpp>
#include <boost/asio/signal_set.hpp>

namespace clips::extension {

struct processData {
    struct Terminal {
        std::string                 router;
        
        boost::process::opstream    in;
        boost::process::ipstream    out;
        std::shared_ptr<boost::process::child> child;
    };
    std::unordered_map<std::string, std::shared_ptr<Terminal>>   terminal_list;
};

#define PROCESS_DATA                USER_ENVIRONMENT_DATA + 3
#define ProcessData(environment)    static_cast<processData*>(GetEnvironmentData(environment, PROCESS_DATA))

void _process_make_terminal(Environment*environment, const char*ROUTER)
{
    auto terminal = std::make_shared<processData::Terminal>();
    terminal->router = ROUTER;
    ProcessData(environment)->terminal_list[ROUTER] = terminal;
    
    /* prepare router for terminal */{
        auto RouterQueryFunction = [](Environment *environment, const char *logicalName, void *context) {
            auto terminal = static_cast<processData::Terminal*>(context);
            return logicalName == terminal->router;
        };
        auto RouterWriteFunction = [](Environment *environment, const char *logicalName, const char *str, void *context){
            auto terminal = static_cast<processData::Terminal*>(context);
            try {
                terminal->in << str << std::flush;
            } catch (std::exception&e) {
                WriteString(environment, STDERR, e.what());
                WriteString(environment, STDERR, "\n");
            }
        };
        auto RouterReadFunction = [](Environment *environment,const char *logicalName,void *context)->int {
            auto terminal = static_cast<processData::Terminal*>(context);
            return terminal->out.get();
        };
        auto RouterUnreadFunction =[](Environment *environment,const char *logicalName,int inchar,void *context)->int {
            auto terminal = static_cast<processData::Terminal*>(context);
            terminal->out.putback(inchar);
            return static_cast<int>(true);
        };
        
        //auto RouterExitFunction = [](Environment *,int,void *){ };
        
        AddRouter(environment, ROUTER, 20,
                  /* RouterQueryFunction  * */RouterQueryFunction,
                  /* RouterWriteFunction  * */RouterWriteFunction,
                  /* RouterReadFunction   * */RouterReadFunction,
                  /* RouterUnreadFunction * */RouterUnreadFunction,
                  /* RouterExitFunction   * */nullptr,
                  /* void                 * */terminal.get());
    }
}

clips::string process_system_output(Environment*environment, const char* shellCommand)
{
    std::future<std::string> output, error;
    
    try {
        boost::asio::io_context io_context;
        boost::process::child c(std::string{shellCommand},
                                //boost::process::std_in.close(),
                                (boost::process::std_out & boost::process::std_err) > output,
                                io_context);
        
        io_context.run();
    } catch (const std::exception&e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    
    return clips::string{output.get()};
}

void process_terminal_start(Environment*environment, const char* ROUTER, const char*SHELL_COMMAND)
{
    try {
        _process_make_terminal(environment, ROUTER);
        auto terminal = ProcessData(environment)->terminal_list.at(ROUTER);
        terminal->child = std::make_shared<boost::process::child>(SHELL_COMMAND,
                                                                  boost::process::std_out > terminal->out,
                                                                  boost::process::std_in  < terminal->in);
        
    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
}
clips::boolean process_terminal_stop(Environment*environment, const char* ROUTER)
{
    try {
        auto terminal = ProcessData(environment)->terminal_list.at(ROUTER);
        DeleteRouter(environment, ROUTER);
        ProcessData(environment)->terminal_list.erase(ROUTER);
        return clips::boolean{true};
    } catch (const std::exception& e) {
        WriteString(environment, STDERR, e.what());
        WriteString(environment, STDERR, "\n");
    }
    return clips::boolean{false};
}

void process_initialize(Environment*environment)
{
    if (nullptr == ProcessData(environment)) {
        AllocateEnvironmentData(environment, PROCESS_DATA, sizeof(processData), [](Environment*environment){
            ProcessData(environment)->~processData();
        });
        new (ProcessData(environment)) processData();
    }
    
    clips::user_function<__LINE__>(environment, "system-output",  process_system_output);
    clips::user_function<__LINE__>(environment, "terminal-start", process_terminal_start);
    clips::user_function<__LINE__>(environment, "terminal-stop",  process_terminal_stop);
}

}// namespace clips::extension {
#endif// CLIPS_EXTENSION_PROCESS_ENABLED
