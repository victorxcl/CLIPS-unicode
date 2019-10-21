//
//  clips.hpp
//  CLIPS_unicode
//
//  Created by chunlei.xiong on 13-11-12.
//  Copyright (c) 2013年 熊春雷. All rights reserved.
//
//  MUST be compiled by C++11 and later compiler.

//  Update at 2019-10-17:
//  1. update import user C++ function to CLIPS with 6.40 beta3 version.

#pragma once

#include <functional>
#include <cassert>
#include <vector>
#include <any>

namespace clips {
    /* The following codes are supported for return values and argument types:
       +------+---------------------------------------------------+
       | Code | Type                                              |
       +------+---------------------------------------------------+
       |  b   | Boolean                                           |
       |  d   | Double Precision Float                            |
       |  e   | External Address                                  |
       |  f   | Fact Address                                      |
       |  i   | Instance Address                                  |
       |  l   | Long Long Integer                                 |
       |  m   | Multifield                                        |
       |  n   | Instance Name                                     |
       |  s   | String                                            |
       |  y   | Symbol                                            |
       |  v   | Void—No Return Value                              |
       |  *   | Any Type                                          |
       +------+---------------------------------------------------+ */
    
    using integer       = long long;
    using real          = double;
    using boolean       = std::tuple<std::string, std::integral_constant<char,'b'>>;
    using string        = std::tuple<std::string, std::integral_constant<char,'s'>>;
    using symbol        = std::tuple<std::string, std::integral_constant<char,'y'>>;
    using instance_name = std::tuple<std::string, std::integral_constant<char,'n'>>;
    using multifield    = std::vector<std::any>;
    // ////////////////////////////////////////////////////////////////////////////////////////
    template<typename>struct type_code      {enum{value='*'};};// * Any Type
    template<>struct type_code<bool>        {enum{value='b'};};// b Boolean
    template<>struct type_code<boolean>     {enum{value='b'};};// b Boolean

    template<>struct type_code<float>       {enum{value='d'};};// d Double Precision Float
    template<>struct type_code<double>      {enum{value='d'};};// d Double Precision Float
    template<>struct type_code<long double> {enum{value='d'};};// d Double Precision Float

    template<>struct type_code</*     */char>      {enum{value='l'};};// l Long Long Integer
    template<>struct type_code</*     */short>     {enum{value='l'};};// l Long Long Integer
    template<>struct type_code</*     */int>       {enum{value='l'};};// l Long Long Integer
    template<>struct type_code</*     */long>      {enum{value='l'};};// l Long Long Integer
    template<>struct type_code</*     */long long> {enum{value='l'};};// l Long Long Integer
    template<>struct type_code<unsigned char>      {enum{value='l'};};// l Long Long Integer
    template<>struct type_code<unsigned short>     {enum{value='l'};};// l Long Long Integer
    template<>struct type_code<unsigned int>       {enum{value='l'};};// l Long Long Integer
    template<>struct type_code<unsigned long>      {enum{value='l'};};// l Long Long Integer
    template<>struct type_code<unsigned long long> {enum{value='l'};};// l Long Long Integer

    template<>struct type_code<      char*>   {enum{value='s'};};// s String
    template<>struct type_code<const char*>   {enum{value='s'};};// s String
    template<>struct type_code<std::string>   {enum{value='s'};};// s String
    template<>struct type_code<string>        {enum{value='s'};};// s String
    template<>struct type_code<symbol>        {enum{value='y'};};// y Symbol
    template<>struct type_code<instance_name> {enum{value='n'};};// n Instance Name
    template<>struct type_code<void>          {enum{value='v'};};// v Void—No Return Value
    //template<>struct type_code<double>      {enum{value='f'};};// f Fact Address
    //template<>struct type_code<short>       {enum{value='i'};};// i Instance Address
    template<>struct type_code<multifield>    {enum{value='m'};};// m Multifield
    //template<typename T>struct type_code<T*>  {enum{value=std::is_same_v<std::remove_const_t<T>,char>?'s':'e'};};// e External Address
    template<typename T>struct type_code<T*>     {enum{value='e'};};// e External Address
    template<typename T>struct type_code<const T>{enum{value=type_code<T>::value};};

    template<typename T>using   return_code = type_code<T>;
    template<typename T>using argument_code = type_code<T>;

    template<typename>struct argument;
#define CLIPS_ARGUMENT_VALUE(float, udfv_contents)                          \
/**/    static float value(Environment*CLIPS, UDFContext *udfc, unsigned i){\
/**/        UDFValue udfv;                                                  \
/**/        UDFNthArgument(udfc, i, argument_code<float>::value, &udfv);    \
/**/        return udfv_contents;                                           \
/**/    }/* CLIPS_ARGUMENT_VALUE */
#define CLIPS_ARGUMENT_TEMPLATE(float, udfv_contents)                       \
/**/    template<>struct argument<float> {                                  \
/**/        CLIPS_ARGUMENT_VALUE(float, udfv_contents)                      \
/**/    };/* CLIPS_ARGUMENT_TEMPLATE */

    CLIPS_ARGUMENT_TEMPLATE(             float, udfv.floatValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(            double, udfv.floatValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(              char, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(             short, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(               int, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(              long, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(         long long, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned      char, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned     short, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned       int, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned      long, udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned long long, udfv.integerValue->contents)


    CLIPS_ARGUMENT_TEMPLATE(              bool,               udfv.lexemeValue != FalseSymbol(CLIPS))
    CLIPS_ARGUMENT_TEMPLATE(           boolean,       boolean{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(       const char*,               udfv.lexemeValue->contents )
    CLIPS_ARGUMENT_TEMPLATE(       std::string,   std::string{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(            string,        string{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(            symbol,        symbol{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(     instance_name, instance_name{udfv.lexemeValue->contents})
        
    template<class T>struct argument<T*> {
        CLIPS_ARGUMENT_VALUE(T*, udfv.externalAddressValue->contents);
    };
    template<class T>struct argument<const T> {
        static const T value(UDFContext *udfc, unsigned i){
            return argument<T>::value(udfc, i);
        }
    };
#undef CLIPS_ARGUMENT_VALUE
#undef CLIPS_ARGUMENT_TEMPLATE
    
    template<char code> struct select_action;
#define CLIPS_SELECT_ACTION(code, CreateValue)                  \
/**/    template<> struct select_action<code> {                 \
/**/        template<typename R> static                         \
/**/        void apply(Environment*CLIPS, UDFValue*udfv, R&&x){ \
/**/            udfv->value = CreateValue/*(CLIPS, x)*/;        \
/**/        }                                                   \
/**/    };/* CLIPS_SELECT_ACTION */
    CLIPS_SELECT_ACTION('b', CreateBoolean/*    */(CLIPS,std::get<0>(x).c_str()))
    CLIPS_SELECT_ACTION('s', CreateString/*     */(CLIPS,std::get<0>(x).c_str()))
    CLIPS_SELECT_ACTION('y', CreateSymbol/*     */(CLIPS,std::get<0>(x).c_str()))
    CLIPS_SELECT_ACTION('n', CreateInstanceName   (CLIPS,std::get<0>(x).c_str()))
    CLIPS_SELECT_ACTION('d', CreateFloat/*      */(CLIPS,x))
    CLIPS_SELECT_ACTION('l', CreateInteger/*    */(CLIPS,x))
    CLIPS_SELECT_ACTION('e', CreateExternalAddress(CLIPS,x))
#undef CLIPS_SELECT_ACTION
    
    template<typename R, typename ... Args>struct build_arguments_code;
    template<typename R, typename A1, typename ... Args>
    struct build_arguments_code<R, A1, Args...> {
        static const char* apply(char code[], unsigned i) {
            code[i+0] = ';';
            code[i+1] = argument_code<A1>::value;
            return build_arguments_code<R,Args...>::apply(code, i+2);
        }
    };
    template<typename R>struct build_arguments_code<R> {
        static const char* apply(char code[], unsigned i) { code[i]='\0'; return code; }
    };
 
    namespace __private{
        template<typename...i>struct m {};
        template<unsigned...i>struct n { enum { value=sizeof...(i), max_value=0xF}; };
        template<unsigned>struct build_n;
        template<>struct build_n<0x1> { typedef n<0x1> type; };
        template<>struct build_n<0x2> { typedef n<0x1,0x2> type; };
        template<>struct build_n<0x3> { typedef n<0x1,0x2,0x3> type; };
        template<>struct build_n<0x4> { typedef n<0x1,0x2,0x3,0x4> type; };
        template<>struct build_n<0x5> { typedef n<0x1,0x2,0x3,0x4,0x5> type; };
        template<>struct build_n<0x6> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6> type; };
        template<>struct build_n<0x7> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7> type; };
        template<>struct build_n<0x8> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8> type; };
        template<>struct build_n<0x9> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9> type; };
        template<>struct build_n<0xA> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA> type; };
        template<>struct build_n<0xB> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB> type; };
        template<>struct build_n<0xC> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC> type; };
        template<>struct build_n<0xD> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xD> type; };
        template<>struct build_n<0xE> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xD,0xE> type; };
        template<>struct build_n<0xF> { typedef n<0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xD,0xE,0xF> type; };
        
        template<typename R,typename A,typename B>struct _invoke;
        template<typename R,typename...Args,int...i> struct _invoke<R, m<Args...>, n<i...>> {
            static R _function(Environment*CLIPS,std::function<R(Args...)>&&theFunc, UDFContext*udfc/*=nullptr*/) {
                return theFunc(argument<Args>::value(CLIPS, udfc, i)...);
            }
        };
    }
    
    template<typename R,typename...Args>
    R invoke_function(Environment*CLIPS,std::function<R(Args...)>&&lambda, UDFContext*udfc/*=nullptr*/) {
        using namespace __private;
        static_assert(sizeof...(Args)<=n<>::max_value, "Error: Only support max 15 arguments in CLIPS's C function");
        return _invoke<R, m<Args...>, typename build_n<sizeof...(Args)>::type>
        /*  */ ::_function(CLIPS, std::move(lambda), udfc);
    }

    template<typename R>
    R invoke_function(Environment*CLIPS,std::function<R()>&&lambda, UDFContext*udfc/*=nullptr*/) {
        return lambda();
    }
    
    template<unsigned i,class R,class...Args> struct is_void_return {
        static std::function<R(Args...)> lambda;
        static void f(Environment*CLIPS, UDFContext *udfc, UDFValue*udfv) {
            enum { code = return_code<R>::value };
            select_action<code>::apply(CLIPS, udfv, invoke_function(CLIPS, std::move(lambda), udfc));
        }
    };
    template<unsigned i,class...Args> struct is_void_return<i, void, Args...> {
        static std::function<void(Args...)>lambda;
        static void f(Environment*CLIPS, UDFContext *udfc, UDFValue*udfv) {
            invoke_function(CLIPS,std::move(lambda), udfc);
        }
    };
    
    template<unsigned i, typename R, typename...Args> std::function<   R(Args...)> is_void_return<i,   R,Args...>::lambda;
    template<unsigned i, /*       */ typename...Args> std::function<void(Args...)> is_void_return<i,void,Args...>::lambda;
    template<unsigned i, typename R, typename...Args>
    void user_function(Environment*CLIPS, const char*name, std::function<R(Args...)>lambda, void*context=nullptr) {
        using namespace __private;
        char argumentsCode[(1+n<>::max_value)*2] = {'*', '\0'};
        build_arguments_code<R, Args...>::apply(argumentsCode, 1);
        char returnCode[2] = {return_code<R>::value, '\0'};
        using UDF = is_void_return<i,R,Args...>;
        UDF::lambda = lambda;
        AddUDFError ok = \
        AddUDF(/* Environment*                   theEnv = */CLIPS,
               /* const char *        clipsFunctionName = */name,
               /* const char *              returnTypes = */returnCode,
               /* unsigned short                minArgs = */sizeof...(Args),
               /* unsigned short                maxArgs = */sizeof...(Args),
               /* const char *            argumentTypes = */argumentsCode,
               /* UserDefinedFunction *cFunctionPointer = */UDF::f,
               /* const char *            cFunctionName = */name,
               /* void *                        context = */context);
        assert(AUE_NO_ERROR == ok);
    };
    
    template<unsigned i, typename R, typename...Args>
    void user_function(Environment*CLIPS,const char*name, R(*f)(Args...), void*context=nullptr) {
        user_function<i>(CLIPS, name, std::function<R(Args...)>(f), context);
    };

}//namespace clips

namespace clips {

    class CLIPS {
        std::shared_ptr<Environment> env;
    public:
        CLIPS() {
            this->env = std::shared_ptr<Environment>(CreateEnvironment(), [](Environment*x){
                DestroyEnvironment(x);
            });
        }
        operator Environment*() { return this->env.get(); }
        inline bool clear() { return Clear(env.get()); }
        inline void reset() { Reset(env.get()); }
        inline long long run(long long steps = -1) { return Run(env.get(), steps); }
        inline bool load(const char*file) {
            LoadError ok = Load(env.get(), file);
            assert(LE_NO_ERROR == ok);
            return LE_NO_ERROR == ok;
        }
        inline bool load_from_string(const char*script) {
            return LoadFromString(env.get(), script, std::strlen(script));
        }
        inline bool batch(const char*file) {
            return Batch(env.get(), file);
        }
        inline bool batch_star(const char*file) {
            return BatchStar(env.get(), file);
        }
        inline void build(const char*script) {
            BuildError ok = Build(env.get(), script);
            assert(BE_NO_ERROR == ok);
        }
        inline std::any eval(const char*script) {
            std::any ret;
            CLIPSValue value;
            EvalError ok = Eval(env.get(), script, &value);
            assert(EE_NO_ERROR == ok);
            if (INTEGER_TYPE == value.header->type) {
                ret = value.integerValue->contents;
            }
            if (FLOAT_TYPE == value.header->type) {
                ret = value.floatValue->contents;
            }
            if (SYMBOL_TYPE == value.header->type) {
                ret = clips::symbol{value.lexemeValue->contents};
            }
            if (STRING_TYPE == value.header->type) {
                ret = clips::string{value.lexemeValue->contents};
            }
            if (INSTANCE_NAME_TYPE == value.header->type) {
                ret = clips::instance_name{value.lexemeValue->contents};
            }
            return ret;
        }
    };
}//namespace clips

#if CLIPS_HPP_TEST_WITH_CATCH_ENABLED
#include <catch2/catch.hpp>
TEST_CASE("expert system CLIPS hello world", "[CLIPS][ExpertSystem]") {
    clips::CLIPS CLIPS;
    REQUIRE(1 +2+3 == std::any_cast<clips::integer>(CLIPS.eval("(+ 1  2 3)")));
    REQUIRE(1.+2+3 == std::any_cast<clips::real   >(CLIPS.eval("(+ 1. 2 3)")));
    
    REQUIRE(clips::string{"2019-10-10"} == std::any_cast<clips::string>(CLIPS.eval("(str-cat 2019 - 10 - 10)")));
    REQUIRE(clips::symbol{"2019-10-10"} == std::any_cast<clips::symbol>(CLIPS.eval("(sym-cat 2019 - 10 - 10)")));
}

TEST_CASE("expert system CLIPS user defined function", "[CLIPS][ExpertSystem]") {
    clips::CLIPS CLIPS;
    SECTION("test build arguments code") {
        using namespace std::string_literals;
        char argumentsCode[128] = {'*', '\0'};
        REQUIRE("*;v"s == clips::build_arguments_code<void,         void>::apply(argumentsCode, 1));
        REQUIRE("*;b"s == clips::build_arguments_code<void,         bool>::apply(argumentsCode, 1));
        REQUIRE("*;l"s == clips::build_arguments_code<void,         char>::apply(argumentsCode, 1));
        REQUIRE("*;l"s == clips::build_arguments_code<void,        short>::apply(argumentsCode, 1));
        REQUIRE("*;l"s == clips::build_arguments_code<void,          int>::apply(argumentsCode, 1));
        REQUIRE("*;l"s == clips::build_arguments_code<void,         long>::apply(argumentsCode, 1));
        REQUIRE("*;l"s == clips::build_arguments_code<void,    long long>::apply(argumentsCode, 1));
        REQUIRE("*;d"s == clips::build_arguments_code<void,        float>::apply(argumentsCode, 1));
        REQUIRE("*;d"s == clips::build_arguments_code<void,       double>::apply(argumentsCode, 1));
        REQUIRE("*;d"s == clips::build_arguments_code<void,  long double>::apply(argumentsCode, 1));
        
        REQUIRE("*;s"s == clips::build_arguments_code<void,        char*>::apply(argumentsCode, 1));
        REQUIRE("*;s"s == clips::build_arguments_code<void,  const char*>::apply(argumentsCode, 1));
        REQUIRE("*;e"s == clips::build_arguments_code<void,        void*>::apply(argumentsCode, 1));
        REQUIRE("*;e"s == clips::build_arguments_code<void, std::string*>::apply(argumentsCode, 1));
        
        REQUIRE("*;b"s == clips::build_arguments_code<void,       clips::boolean>::apply(argumentsCode, 1));
        REQUIRE("*;s"s == clips::build_arguments_code<void,        clips::string>::apply(argumentsCode, 1));
        REQUIRE("*;y"s == clips::build_arguments_code<void,        clips::symbol>::apply(argumentsCode, 1));
        REQUIRE("*;n"s == clips::build_arguments_code<void, clips::instance_name>::apply(argumentsCode, 1));
        
#define TEST_ARGUMENTS bool, int, float, const char*, std::string, clips::string, clips::boolean, clips::symbol, clips::instance_name
        clips::user_function<__LINE__>(CLIPS, "test", static_cast<void(*)(TEST_ARGUMENTS)>([](TEST_ARGUMENTS){ }));
        REQUIRE("*;b;l;d;s;s;s;b;y;n"s == clips::build_arguments_code<void, TEST_ARGUMENTS>::apply(argumentsCode, 1));
#undef  TEST_ARGUMENTS
    }
    
    clips::user_function<__LINE__>(CLIPS, "hello", static_cast<clips::string(*)()>([]{ return clips::string{"hello"}; }));
    clips::user_function<__LINE__>(CLIPS, "world", static_cast<clips::string(*)()>([]{ return clips::string{"world"}; }));
    
    REQUIRE(clips::string{"hello"} == std::any_cast<clips::string>(CLIPS.eval("(hello)")));
    REQUIRE(clips::string{"world"} == std::any_cast<clips::string>(CLIPS.eval("(world)")));
    REQUIRE(clips::string{"helloworld"} == std::any_cast<clips::string>(CLIPS.eval("(str-cat (hello) (world))")));
}
#endif//CLIPS_HPP_TEST_WITH_CATCH_ENABLED
