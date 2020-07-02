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
#include <string>
#include <any>
#include <tuple>
#include <memory>
#include <cstring>

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

    template<typename T, char __c__>
    struct cxx_proxy_type_with_tag:std::pair<T, std::integral_constant<char,__c__>> {
        using tag       = std::integral_constant<char,__c__>;
        using self_type = cxx_proxy_type_with_tag<T,__c__>;
        
        cxx_proxy_type_with_tag() = default;
        explicit cxx_proxy_type_with_tag(const std::string&x):std::pair<T, tag>(x, tag()) {}
        explicit cxx_proxy_type_with_tag(            void* x):std::pair<T, tag>(x, tag()) {}
        friend std::ostream& operator<<(std::ostream&os, const self_type&x) {
            return os << std::get<0>(x);
        }
    };
    
    using integer           = long long;
    using real              = double;
    using boolean           = bool;//std::tuple<bool,        std::integral_constant<char,'b'>>;
//    using string            = std::tuple<std::string, std::integral_constant<char,'s'>>;
//    using symbol            = std::tuple<std::string, std::integral_constant<char,'y'>>;
//    using instance_name     = std::tuple<std::string, std::integral_constant<char,'n'>>;
//    using external_address  = std::tuple<void*,       std::integral_constant<char,'e'>>;
    using string            = cxx_proxy_type_with_tag<std::string, 's'>;
    using symbol            = cxx_proxy_type_with_tag<std::string, 'y'>;
    using instance_name     = cxx_proxy_type_with_tag<std::string, 'n'>;
    using external_address  = cxx_proxy_type_with_tag<      void*, 'e'>;
    using multifield        = std::vector<std::any>;
    // ////////////////////////////////////////////////////////////////////////////////////////
    template<typename>struct type_code      {enum{value='*', expect_bits=0};};// * Any Type
    template<>struct type_code<bool>        {enum{value='b', expect_bits=BOOLEAN_BIT};};// b Boolean
//    template<>struct type_code<boolean>     {enum{value='b', expect_bits=BOOLEAN_BIT};};// b Boolean

    template<>struct type_code<float>       {enum{value='d', expect_bits=FLOAT_BIT};};// d Double Precision Float
    template<>struct type_code<double>      {enum{value='d', expect_bits=FLOAT_BIT};};// d Double Precision Float
    template<>struct type_code<long double> {enum{value='d', expect_bits=FLOAT_BIT};};// d Double Precision Float

    template<>struct type_code</*     */char>      {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code</*     */short>     {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code</*     */int>       {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code</*     */long>      {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code</*     */long long> {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code<unsigned char>      {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code<unsigned short>     {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code<unsigned int>       {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code<unsigned long>      {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer
    template<>struct type_code<unsigned long long> {enum{value='l', expect_bits=INTEGER_BIT};};// l Long Long Integer

    template<>struct type_code<      char*>        {enum{value='s', expect_bits=STRING_BIT|SYMBOL_BIT};};// s String, y Symbol
    template<>struct type_code<const char*>        {enum{value='s', expect_bits=STRING_BIT|SYMBOL_BIT};};// s String, y Symbol
    template<>struct type_code<std::string>        {enum{value='s', expect_bits=STRING_BIT|SYMBOL_BIT};};// s String, y Symbol
    template<>struct type_code<string>             {enum{value='s', expect_bits=STRING_BIT           };};// s String
    template<>struct type_code<symbol>             {enum{value='y', expect_bits=SYMBOL_BIT           };};// y Symbol
    template<>struct type_code<instance_name>      {enum{value='n', expect_bits=INSTANCE_NAME_BIT    };};// n Instance Name
    template<>struct type_code<void>               {enum{value='v', expect_bits=VOID_BIT             };};// v Void—No Return Value
  //template<>struct type_code<double>             {enum{value='f', expect_bits=VOID_BIT             };};// f Fact Address
  //template<>struct type_code<short>              {enum{value='i', expect_bits=VOID_BIT             };};// i Instance Address
    template<>struct type_code<multifield>         {enum{value='m', expect_bits=MULTIFIELD_BIT       };};// m Multifield
    template<>struct type_code<external_address>   {enum{value='e', expect_bits=EXTERNAL_ADDRESS_BIT };};// e External Address
    template<typename T>struct type_code<T*>       {enum{value='e', expect_bits=EXTERNAL_ADDRESS_BIT };};// e External Address

    template<typename T>struct type_code<const T>{ enum{
        value=type_code<T>::value,
        expect_bits=type_code<T>::expect_bits
    };};

    template<typename T>using   return_code = type_code<T>;
    template<typename T>using argument_code = type_code<T>;

    template<typename T>struct argument;
    template<typename T>struct primitive_value;
#define CLIPS_ARGUMENT_VALUE(float, udfv_contents)                              \
/**/    static float value(UDFContext *udfc, unsigned i){                       \
/**/        UDFValue udfv;                                                      \
/**/        UDFNthArgument(udfc, i, argument_code<float>::expect_bits, &udfv);  \
/**/        return udfv_contents;                                               \
/**/    }/* CLIPS_ARGUMENT_VALUE */
#define CLIPS_ARGUMENT_TEMPLATE(float, udfv_contents)                           \
/**/    template<>struct argument<float> {                                      \
/**/        CLIPS_ARGUMENT_VALUE(float, udfv_contents)                          \
/**/    };                                                                      \
/**/    template<>struct primitive_value<float> {                               \
/**/        static float apply(UDFContext *udfc, const CLIPSValue&udfv) {       \
/**/            return udfv_contents;                                           \
/**/        }                                                                   \
/**/    };/* CLIPS_ARGUMENT_TEMPLATE */

    CLIPS_ARGUMENT_TEMPLATE(             float, /*                      */udfv.floatValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(            double, /*                      */udfv.floatValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(              char, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(             short, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(               int, static_cast<         int>(udfv.integerValue->contents))
    CLIPS_ARGUMENT_TEMPLATE(              long, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(         long long, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned      char, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned     short, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned       int, static_cast<unsigned int>(udfv.integerValue->contents))
    CLIPS_ARGUMENT_TEMPLATE(unsigned      long, /*                      */udfv.integerValue->contents)
    CLIPS_ARGUMENT_TEMPLATE(unsigned long long, /*                      */udfv.integerValue->contents)

    CLIPS_ARGUMENT_TEMPLATE(              bool,               udfv.lexemeValue != FalseSymbol(udfc->environment))
//  CLIPS_ARGUMENT_TEMPLATE(           boolean,       boolean{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(       const char*,               udfv.lexemeValue->contents )
    CLIPS_ARGUMENT_TEMPLATE(       std::string,   std::string{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(            string,        string{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(            symbol,        symbol{udfv.lexemeValue->contents})
    CLIPS_ARGUMENT_TEMPLATE(     instance_name, instance_name{udfv.lexemeValue->contents})

    template<>struct primitive_value<clips::multifield> {
        static clips::multifield apply(UDFContext*udfc, const UDFValue&udfv) {
            clips::multifield multifield;
            for (int i=0; i<udfv.multifieldValue->length; i++) {
                CLIPSValue&v = udfv.multifieldValue->contents[i];
                /*  */ if (v.header->type == FLOAT_TYPE) {
                    auto&&x = primitive_value<clips::real>::apply(udfc, v);
                    multifield.push_back(x);
                } else if (v.header->type == INTEGER_TYPE) {
                    auto&&x = primitive_value<clips::integer>::apply(udfc, v);
                    multifield.push_back(x);
                } else if (v.header->type == SYMBOL_TYPE) {
                    auto&&x = primitive_value<clips::symbol>::apply(udfc, v);
                    multifield.push_back(x);
                } else if (v.header->type == STRING_TYPE) {
                    auto&&x = primitive_value<clips::string>::apply(udfc, v);
                    multifield.push_back(x);
                } else if (v.header->type == INSTANCE_NAME_TYPE) {
                    auto&&x = primitive_value<clips::instance_name>::apply(udfc, v);
                    multifield.push_back(x);
                }
            }
            return multifield;
        }
    };

    template<>struct argument<clips::multifield> {
        static clips::multifield value(UDFContext *udfc, unsigned i){
            UDFValue udfv;
            UDFNthArgument(udfc, i, argument_code<clips::multifield>::expect_bits, &udfv);
            return primitive_value<clips::multifield>::apply(udfc, udfv);
        }
    };

    template<class T>struct argument<T*> {
        CLIPS_ARGUMENT_VALUE(T*, static_cast<T*>(udfv.externalAddressValue->contents));
    };
    template<class T>struct argument<const T> {
        static const T value(/* Environment*CLIPS, */UDFContext *udfc, unsigned i){
            return argument<T>::value(udfc, i);
        }
        static const T value(   Environment*CLIPS,   UDFContext *udfc, unsigned i){
            return argument<T>::value(CLIPS, udfc, i);
        }
    };
    template<typename T>struct argument<T&> {
        static T value(/* Environment*CLIPS, */UDFContext *udfc, unsigned i){
            return argument<T>::value(udfc, i);
        }
        static T value(   Environment*CLIPS,   UDFContext *udfc, unsigned i){
            return argument<T>::value(CLIPS, udfc, i);
        }
    };
#undef CLIPS_ARGUMENT_VALUE
#undef CLIPS_ARGUMENT_TEMPLATE
    
    template<char code> struct create_primitive_value;
#define CLIPS_CREATE_PRIMITIVE_VALUE(code, CreateValue)         \
/**/    template<> struct create_primitive_value<code> {        \
/**/        template<typename R> static                         \
/**/        void apply(Environment*CLIPS, UDFValue*udfv, R&&x){ \
/**/            udfv->value = CreateValue/*(CLIPS, x)*/;        \
/**/        }                                                   \
/**/    };/* CLIPS_CREATE_PRIMITIVE_VALUE */
    CLIPS_CREATE_PRIMITIVE_VALUE('b', CreateBoolean/*     */(CLIPS,x))
    CLIPS_CREATE_PRIMITIVE_VALUE('s', CreateString/*      */(CLIPS,std::get<0>(x).c_str()))
    CLIPS_CREATE_PRIMITIVE_VALUE('y', CreateSymbol/*      */(CLIPS,std::get<0>(x).c_str()))
    CLIPS_CREATE_PRIMITIVE_VALUE('n', CreateInstanceName/**/(CLIPS,std::get<0>(x).c_str()))
    CLIPS_CREATE_PRIMITIVE_VALUE('d', CreateFloat/*       */(CLIPS,x))
    CLIPS_CREATE_PRIMITIVE_VALUE('l', CreateInteger/*     */(CLIPS,x))
    CLIPS_CREATE_PRIMITIVE_VALUE('e', CreateCExternalAddress(CLIPS,std::get<0>(x)/*    */))

    template<> struct create_primitive_value<return_code<multifield>::value> {
        static void apply(Environment*CLIPS, UDFValue*udfv, const multifield&x){
            MultifieldBuilder* mBuilder = CreateMultifieldBuilder(CLIPS, x.size());
            for (auto&&y:x) {
                if (y.has_value()) {
                    /*  */ if (typeid(integer) == y.type()) {
                        MBAppendInteger(mBuilder, std::any_cast<integer>(y));
                    } else if (typeid(real) == y.type()) {
                        MBAppendFloat(mBuilder, std::any_cast<real>(y));
                    } else if (typeid(boolean) == y.type()) {
                        if (std::any_cast<boolean>(y)) {
                            MBAppendCLIPSLexeme(mBuilder, TrueSymbol(CLIPS));
                        } else {
                            MBAppendCLIPSLexeme(mBuilder, FalseSymbol(CLIPS));
                        }
                    } else if (typeid(string) == y.type()) {
                        MBAppendString(mBuilder, std::get<0>(std::any_cast<string>(y)).c_str());
                    } else if (typeid(symbol) == y.type()) {
                        MBAppendSymbol(mBuilder, std::get<0>(std::any_cast<symbol>(y)).c_str());
                    } else if (typeid(instance_name) == y.type()) {
                        MBAppendInstanceName(mBuilder, std::get<0>(std::any_cast<instance_name>(y)).c_str());
                    } else if (typeid(external_address) == y.type()) {
                        CLIPSExternalAddress*externalAddress = CreateCExternalAddress(CLIPS, std::get<0>(std::any_cast<external_address>(y)));
                        MBAppendCLIPSExternalAddress(mBuilder, externalAddress);
                    }
                }
            }
            udfv->value = MBCreate(mBuilder);
            MBDispose(mBuilder);
        }
    };
#undef CLIPS_CREATE_PRIMITIVE_VALUE
    
    template<typename R, typename ... Args>struct build_arguments_code;
    template<typename R, typename A1, typename ... Args>
    struct build_arguments_code<R, A1, Args...> {
        static const std::string& apply(std::string&code, unsigned long i) {
            if (i <= 1) { code = "*"; }
            std::string currentCode = ";";
            if constexpr(std::is_same_v<const char*, A1> || std::is_same_v<char*, A1> ||
                         std::is_same_v<std::string, std::remove_reference_t<std::remove_cv_t<A1>>>) {
                currentCode += argument_code<clips::string>::value;
                currentCode += argument_code<clips::symbol>::value;
            } else {
                currentCode += argument_code<A1>::value;
            }
            return build_arguments_code<R,Args...>::apply(code+=currentCode, i+std::size(currentCode));
        }
    };
    template<typename R>struct build_arguments_code<R> {
        static const std::string& apply(std::string&code, unsigned long i) {
            return code;
        }
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
        
    template<typename R, typename, typename A, typename B>struct _invoke;
        template<typename R, typename...Args, int...i>
        struct _invoke<R, std::integral_constant<int, 0>, m<Args...>, n<i...>> {
            static R _function(std::function<R(/*        */ Args...)>&&theFunc, UDFContext*udfc/* =nullptr */) {
                return theFunc(argument<Args>::value(udfc, i)...);
            }
        };
        template<typename R, typename...Args, int...i>
        struct _invoke<R,                    UDFContext*, m<Args...>, n<i...>> {
            static R _function(std::function<R(UDFContext*, Args...)>&&theFunc, UDFContext*udfc/* =nullptr */) {
                return theFunc(udfc, argument<Args>::value(udfc, i)...);
            }
        };
    }
    
    template<typename R, typename...Args>
    R invoke_function(std::function<R(Args...)>&&lambda, UDFContext*udfc/*=nullptr*/) {
        using namespace __private;
        static_assert(sizeof...(Args)<=n<>::max_value, "Error: Only support max 15 arguments in CLIPS's C function");
        return _invoke<R, std::integral_constant<int,0>, m<Args...>, typename build_n<sizeof...(Args)>::type>
        /*  */ ::_function(std::move(lambda), udfc);
    }
    template<typename R, typename...Args>
    R invoke_function(std::function<R(UDFContext*,Args...)>&&lambda, UDFContext*udfc/*=nullptr*/) {
        using namespace __private;
        static_assert(sizeof...(Args)<=n<>::max_value, "Error: Only support max 15 arguments in CLIPS's C function");
        return _invoke<R, UDFContext*, m<Args...>, typename build_n<sizeof...(Args)>::type>
        /*  */ ::_function(std::move(lambda), udfc);
    }

    template<typename R>
    R invoke_function(std::function<R(UDFContext*)>&&lambda, UDFContext*udfc/*=nullptr*/) {
        return lambda(udfc);
    }

    template<typename R>
    R invoke_function(std::function<R()>&&lambda, UDFContext*udfc/*=nullptr*/) {
        return lambda();
    }
    
    template<unsigned i, class R, class...Args> struct is_void_return {
        static std::function<R(Args...)> lambda;
        static void f(Environment*environment, UDFContext *udfc, UDFValue*udfv) {
            enum { code = return_code<R>::value };
            create_primitive_value<code>::apply(environment, udfv, invoke_function(std::move(lambda), udfc));
        }
    };
    template<unsigned i, /*    */ class...Args> struct is_void_return<i, void, Args...> {
        static std::function<void(Args...)>lambda;
        static void f(Environment*environment, UDFContext *udfc, UDFValue*udfv) {
            invoke_function(std::move(lambda), udfc);
        }
    };
    template<unsigned i, class R, class...Args> struct is_void_return<i, R, UDFContext*, Args...> {
        static std::function<R(UDFContext*, Args...)> lambda;
        static void f(Environment*environment, UDFContext *udfc, UDFValue*udfv) {
            enum { code = return_code<R>::value };
            create_primitive_value<code>::apply(environment, udfv, invoke_function(std::move(lambda), udfc));
        }
    };
    template<unsigned i, /*    */ class...Args> struct is_void_return<i, void, UDFContext*, Args...> {
        static std::function<void(UDFContext*, Args...)>lambda;
        static void f(Environment*environment, UDFContext *udfc, UDFValue*udfv) {
            invoke_function(std::move(lambda), udfc);
        }
    };
    
    template<unsigned i, typename R, typename...Args> std::function<   R(/*         */Args...)> is_void_return<i,    R, Args...>::lambda;
    template<unsigned i, /*       */ typename...Args> std::function<void(/*         */Args...)> is_void_return<i, void, Args...>::lambda;
    template<unsigned i, typename R, typename...Args> std::function<   R(UDFContext*, Args...)> is_void_return<i,    R, UDFContext*, Args...>::lambda;
    template<unsigned i, /*       */ typename...Args> std::function<void(UDFContext*, Args...)> is_void_return<i, void, UDFContext*, Args...>::lambda;

    template<unsigned i, typename R, typename...Args>
    void user_function(Environment*CLIPS, const char*name, std::function<R(Args...)>&&lambda, void*context=nullptr) {
        using Tuple = std::tuple<Args...>;
        if constexpr (std::tuple_size<Tuple>::value > 0) {
            using T_firstArgument = std::tuple_element_t<0, Tuple>;
            static_assert(!std::is_same_v<Environment*, std::remove_reference_t<std::remove_cv_t<T_firstArgument>>>,
                          "Error: clips.hpp only allow UDFContext* as the first argument of user defined function.");
        }
        
        using namespace __private;
        std::string argumentsCode = "*";
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
               /* const char *            argumentTypes = */argumentsCode.c_str(),
               /* UserDefinedFunction *cFunctionPointer = */UDF::f,
               /* const char *            cFunctionName = */name,
               /* void *                        context = */context);
        assert(AUE_NO_ERROR == ok);
    };
    template<unsigned i, typename R, typename...Args>
    void user_function(Environment*CLIPS, const char*name, std::function<R(UDFContext*, Args...)>&&lambda, void*context=nullptr) {
        using namespace __private;
        std::string argumentsCode = "*";
        build_arguments_code<R, Args...>::apply(argumentsCode, 1);// 参数数量和上面的不一样
        char returnCode[2] = {return_code<R>::value, '\0'};
        using UDF = is_void_return<i, R, UDFContext*, Args...>; // 这里也和上面不一样
        UDF::lambda = lambda;
        AddUDFError ok = \
        AddUDF(/* Environment*                   theEnv = */CLIPS,
               /* const char *        clipsFunctionName = */name,
               /* const char *              returnTypes = */returnCode,
               /* unsigned short                minArgs = */sizeof...(Args),
               /* unsigned short                maxArgs = */sizeof...(Args),
               /* const char *            argumentTypes = */argumentsCode.c_str(),
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

    class CLIPS final {
        std::shared_ptr<Environment> env;
    public:
        CLIPS():CLIPS(nullptr) {}
        explicit CLIPS(Environment*environment) {
            if (nullptr == environment) {
                this->env = std::shared_ptr<Environment>(CreateEnvironment(), [](Environment*x){
                    DestroyEnvironment(x);
                });
            } else {
                this->env = std::shared_ptr<Environment>(environment, [](Environment*x){
                    
                });
            }
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
            if (EE_NO_ERROR != Eval(env.get(), script, &value)) {
                WriteString(*this, STDERR, "[ERROR] eval script: \n");
                WriteString(*this, STDERR, script);
                WriteString(*this, STDERR, "\n");
            } else {
                ret = from_clips_value(value);
            }
            return ret;
        }
        std::any from_clips_value(CLIPSValue value)
        {
            if (INTEGER_TYPE == value.header->type) {
                return value.integerValue->contents;
            }
            if (FLOAT_TYPE == value.header->type) {
                return value.floatValue->contents;
            }
            if (SYMBOL_TYPE == value.header->type) {
                /*  */ if (value.lexemeValue == TrueSymbol(this->env.get())) {
                    return clips::boolean{true};
                } else if (value.lexemeValue == FalseSymbol(this->env.get())) {
                    return clips::boolean{false};
                } else {
                    return clips::symbol{value.lexemeValue->contents};
                }
            }
            if (STRING_TYPE == value.header->type) {
                return clips::string{value.lexemeValue->contents};
            }
            if (INSTANCE_NAME_TYPE == value.header->type) {
                return clips::instance_name{value.lexemeValue->contents};
            }
            if (EXTERNAL_ADDRESS_TYPE == value.header->type) {
                return value.externalAddressValue->contents;
            }
            if (MULTIFIELD_TYPE == value.header->type) {
                clips::multifield ret;
                for (int i=0; i<value.multifieldValue->length; i++) {
                    ret.push_back(from_clips_value(value.multifieldValue->contents[i]));
                }
                return ret;
            }
            return clips::boolean{false};
        }
    };

    template<unsigned i, typename ... Args>
    void user_function(std::shared_ptr<CLIPS>clips, Args && ... args)
    {
        user_function<i>(static_cast<Environment*>(*clips), std::forward<Args>(args)...);
    }
}//namespace clips

#ifndef CLIPS_EXTENSION_TEST_BENCH_ENABLED
#   define CLIPS_EXTENSION_TEST_BENCH_ENABLED 1
#endif//CLIPS_EXTENSION_TEST_BENCH_ENABLED

#ifndef CLIPS_EXTENSION_UTILITY_ENABLED
#   define CLIPS_EXTENSION_UTILITY_ENABLED 1
#endif//CLIPS_EXTENSION_UTILITY_ENABLED

#ifndef CLIPS_EXTENSION_SOCKET_ENABLED
#   define CLIPS_EXTENSION_SOCKET_ENABLED 1
#endif//CLIPS_EXTENSION_SOCKET_ENABLED

#ifndef CLIPS_EXTENSION_ZEROMQ_ENABLED
#   define CLIPS_EXTENSION_ZEROMQ_ENABLED 1
#endif//CLIPS_EXTENSION_ZEROMQ_ENABLED

#ifndef CLIPS_EXTENSION_MUSTACHE_ENABLED
#   define CLIPS_EXTENSION_MUSTACHE_ENABLED 1
#endif//CLIPS_EXTENSION_MUSTACHE_ENABLED

#ifndef CLIPS_EXTENSION_PROCESS_ENABLED
#   define CLIPS_EXTENSION_PROCESS_ENABLED 1
#endif//CLIPS_EXTENSION_PROCESS_ENABLED

namespace clips::extension {

#if CLIPS_EXTENSION_TEST_BENCH_ENABLED
    void test_benchmark_initialize(Environment*environment);
    void test_benchmark();
#endif//CLIPS_EXTENSION_TEST_BENCH_ENABLED

#if CLIPS_EXTENSION_UTILITY_ENABLED
    void utility_initialize(Environment*environment);
#endif//CLIPS_EXTENSION_UTILITY_ENABLED

#if CLIPS_EXTENSION_SOCKET_ENABLED
    void socket_initialize(Environment*environment);
#endif// CLIPS_EXTENSION_SOCKET_ENABLED

#if CLIPS_EXTENSION_ZEROMQ_ENABLED
    void zeromq_initialize(Environment*environment);
#endif// CLIPS_EXTENSION_ZEROMQ_ENABLED

#if CLIPS_EXTENSION_MUSTACHE_ENABLED
    void mustache_initialize(Environment*environment);
#endif// CLIPS_EXTENSION_MUSTACHE_ENABLED

#if CLIPS_EXTENSION_PROCESS_ENABLED
    void process_initialize(Environment*environment);
#endif// CLIPS_EXTENSION_PROCESS_ENABLED

}// namespace clips::extension {


