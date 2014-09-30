//
//  clips.hpp
//  CLIPS_unicode
//
//  Created by chunlei.xiong on 13-11-12.
//  Copyright (c) 2013年 熊春雷. All rights reserved.
//
//  必须在支持C++11语法的编译器下面使用

#pragma once

extern "C"{
// 下面的代码有可能采用C++的方式进行编译
#   include "clips.h"
}

#include <functional>
#include <tuple>

namespace clips {
    
    /********************************************************************************
     Return Code	Return Type Expected
     a          	External Address
     b          	Boolean
     c          	Character
     d          	Double Precision Float
     f          	Single Precision Float
     i          	Integer
     j          	Unknown Data Type (Symbol, String, or Instance Name Expected)
     k          	Unknown Data Type (Symbol or String Expected)
     l          	Long Integer
     m          	Multifield
     n          	Unknown Data Type (Integer or Float Expected)
     o          	Instance Name
     s          	String
     u          	Unknown Data Type (Any Type Expected)
     v          	Void—No Return Value
     w          	Symbol
     x          	Instance Address
     
     Type Code   Allowed Types
     a           External Address
     d           Float
     e           Instance Address, Instance Name, or Symbol
     f           Float
     g           Integer, Float, or Symbol
     h           Instance Address, Instance Name, Fact Address, Integer, or Symbol
     i           Integer
     j           Symbol, String, or Instance Name
     k           Symbol or String
     l           Integer
     m           Multifield
     n           Integer or Float
     o           Instance Name
     p           Instance Name or Symbol
     q           Symbol, String, or Multifield
     s           String
     u           Any Data Type
     w           Symbol
     x           Instance Address
     y           Fact Address
     z           Fact address, Integer, or Symbol
     ******************************************************************************/
    
    //template<class T,...>struct multi_field;
    
    //////////////////////////////////////////////////////////////////////////////////////////
    template<class>struct return_code;
    //{static char value(){return 'u';}};// u          	Unknown Data Type (Any Type Expected)
    template<>struct return_code<bool>   {enum{value='b'};};// b          	Boolean
    template<>struct return_code<char>   {enum{value='c'};};// c          	Character
    template<>struct return_code<float>  {enum{value='f'};};// f          	Single Precision Float
    template<>struct return_code<double> {enum{value='d'};};// d          	Double Precision Float
    template<>struct return_code<short>  {enum{value='i'};};// i          	Integer
    template<>struct return_code<int>    {enum{value='i'};};// i          	Integer
    template<>struct return_code<long>   {enum{value='l'};};// l          	Long Integer
    template<>struct return_code<char*>  {enum{value='s'};};// s          	String
    template<>struct return_code<void>   {enum{value='v'};};// v          	Void—No Return Value
    
    template<>struct return_code<unsigned char>  {enum{value='c'};};// c          	Character
    template<>struct return_code<unsigned short> {enum{value='i'};};// i          	Integer
    template<>struct return_code<unsigned int>   {enum{value='i'};};// i          	Integer
    template<>struct return_code<unsigned long>  {enum{value='l'};};// l          	Long Integer
    
    template<class T>struct return_code<T*> {enum{value='a'};};// a          	External Address
    
    template<class T>struct return_code<const T> {enum{value=return_code<T>::value};};// const [int|long|char|float|double]
    template<>struct return_code<const char*>  {enum{value='s'};};// s          	String
    
    // 下面是还没有用到的返回值code
    //j          	Unknown Data Type (Symbol, String, or Instance Name Expected)
    //k          	Unknown Data Type (Symbol or String Expected)
    //m          	Multifield
    //n          	Unknown Data Type (Integer or Float Expected)
    //o          	Instance Name
    //w          	Symbol
    //x          	Instance Address
    //////////////////////////////////////////////////////////////////////////////////////////
    template<class>struct argument;
    //{static char value(){return 'u';}};// u           Any Data Type
    template<>struct argument<float>
    {
        enum{code = 'f'};// f           Float
        static float value(void*theEnv,int i){return EnvRtnDouble(theEnv,i);}
    };
    template<>struct argument<double>
    {
        enum{code = 'd'};// d           Float
        static double value(void*theEnv,int i){return EnvRtnDouble(theEnv,i);}
    };
    template<>struct argument<short>
    {
        enum{code = 'i'};// i           Integer
        static int value(void*theEnv,int i){return (int)EnvRtnLong(theEnv,i);}
    };
    template<>struct argument<int>
    {
        enum{code = 'i'};// i           Integer
        static int value(void*theEnv,int i){return (int)EnvRtnLong(theEnv,i);}
    };
    template<>struct argument<long>
    {
        enum{code = 'l'};// l           Integer
        static long value(void*theEnv,int i){return EnvRtnLong(theEnv,i);}
    };
    template<>struct argument<char*>
    {
        enum{code = 's'};// s           String
        static char* value(void*theEnv,int i){return EnvRtnLexeme(theEnv,i);}
    };
    
    template<>struct argument<unsigned short>
    {
        enum{code = 'i'};// i           Integer
        static int value(void*theEnv,int i){return (int)EnvRtnLong(theEnv,i);}
    };
    template<>struct argument<unsigned int>
    {
        enum{code = 'i'};// i           Integer
        static int value(void*theEnv,int i){return (int)EnvRtnLong(theEnv,i);}
    };
    template<>struct argument<unsigned long>
    {
        enum{code = 'l'};// l           Integer
        static long value(void*theEnv,int i){return EnvRtnLong(theEnv,i);}
    };
    
    template<class T>struct argument<T*>
    {
        enum{code = 'a'};// a          	External Address
        static T* value(void*theEnv,int i)
        {DATA_OBJECT DO; return (T*)DOPToPointer(EnvRtnUnknown(theEnv,i,&DO));}
    };

    template<class T>struct argument<const T>
    {
        static char code(){return argument<T>::code();}// const [int|long|char|float|double]
        static const T value(void*theEnv,int i){return argument<T>::value(theEnv,i);}
    };
    // 下面是还没有用到的参数值的code
    //e           Instance Address, Instance Name, or Symbol
    //g           Integer, Float, or Symbol
    //h           Instance Address, Instance Name, Fact Address, Integer, or Symbol
    //j           Symbol, String, or Instance Name
    //k           Symbol or String
    //m           Multifield
    //n           Integer or Float
    //o           Instance Name
    //p           Instance Name or Symbol
    //q           Symbol, String, or Multifield
    //w           Symbol
    //x           Instance Address
    //y           Fact Address
    //z           Fact address, Integer, or Symbol
    //////////////////////////////////////////////////////////////////////////////////////////
    
    template<char code> struct select_action;
    template<> struct select_action<'s'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddSymbol(theEnv, const_cast<char*>(t));
            returnValue->type = STRING;
        }
    };
    template<> struct select_action<'f'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddDouble(theEnv, t);
            returnValue->type = FLOAT;
        }
    };
    template<> struct select_action<'d'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddDouble(theEnv, t);
            returnValue->type = FLOAT;
        }
    };
    template<> struct select_action<'l'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddLong(theEnv, t);
            returnValue->type = INTEGER;
        }
    };
    template<> struct select_action<'i'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddLong(theEnv, t);
            returnValue->type = INTEGER;
        }
    };
    template<> struct select_action<'c'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = EnvAddLong(theEnv, t);
            returnValue->type = INTEGER;
        }
    };
    template<> struct select_action<'b'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = t?SymbolData(theEnv)->TrueSymbol:SymbolData(theEnv)->FalseSymbol;
            returnValue->type = SYMBOL;
        }
    };
    template<> struct select_action<'a'>
    {
        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
        {
            returnValue->value = t;
            returnValue->type = EXTERNAL_ADDRESS;
        }
    };

//    template<> struct select_action<'v'>
//    {
//        template<class R> static void work(void*theEnv, DATA_OBJECT_PTR returnValue, R t)
//        {
//            returnValue->type = RVOID;
//            returnValue->value = SymbolData(theEnv)->FalseSymbol;
//        }
//    };
    
    template<typename R, typename ... Args>struct build_arguments_code;
    template<typename R, typename A1, typename ... Args>
    struct build_arguments_code<R,A1,Args...>
    {
        static void work(char code[],int i)
        {
            code[sizeof...(Args)+i]=argument<A1>::code;
            build_arguments_code<R,Args...>::work(code,i+1);
        }
    };
    template<typename R>struct build_arguments_code<R>
    {
        static void work(char code[],int i) {code[i]='\0';}
    };
 
    namespace _private{
        template<typename...i>struct m {};
        template<int...i>struct n { enum {value=sizeof...(i)}; };
        template<int>struct build_n;
        template<>struct build_n<1> { typedef n<1> type; };
        template<>struct build_n<2> { typedef n<1,2> type; };
        template<>struct build_n<3> { typedef n<1,2,3> type; };
        template<>struct build_n<4> { typedef n<1,2,3,4> type; };
        template<>struct build_n<5> { typedef n<1,2,3,4,5> type; };
        template<>struct build_n<6> { typedef n<1,2,3,4,5,6> type; };
        template<>struct build_n<7> { typedef n<1,2,3,4,5,6,7> type; };
        template<>struct build_n<8> { typedef n<1,2,3,4,5,6,7,8> type; };
        template<>struct build_n<9> { typedef n<1,2,3,4,5,6,7,8,9> type; };
        
        template<typename R,typename A,typename B>struct _call;
        template<typename R,typename...Args,int...i> struct _call<R,m<Args...>,n<i...>>
        {
            static R _function(void*theEnv,std::function<R(Args...)>&&theFunc)
            {
                return std::forward<R>(theFunc(argument<Args>::value(theEnv, i)...));
            }
        };
    }
    
    template<typename R,typename...Args> R call_function(void*theEnv,std::function<R(Args...)>&&theFunc)
    {
        using namespace _private;
        static_assert(sizeof...(Args)<=9, "CLIPS的C函数的限定符最多支持到9个参数，所以这里也就限定为最多9个参数");
        return _call<R,m<Args...>,typename build_n<sizeof...(Args)>::type>::_function(theEnv,std::move(theFunc));
    }

    template<typename R> R call_function(void*theEnv,std::function<R()>&&theFunc) { return theFunc(); }
    
    template<int i,class R,class...Args> struct is_void_return
    {
        static std::function<R(Args...)>theFunc;
        static void f(void*theEnv, DATA_OBJECT_PTR returnValue)
        {
            enum { code = return_code<R>::value};
            select_action<code>::work(theEnv, returnValue, call_function(theEnv,std::move(theFunc)));
        }
    };
    template<int i,class...Args> struct is_void_return<i,void,Args...>
    {
        static std::function<void(Args...)>theFunc;
        static void f(void*theEnv, DATA_OBJECT_PTR returnValue)
        {
            call_function(theEnv,std::move(theFunc));
            returnValue->type = RVOID;
            returnValue->value = SymbolData(theEnv)->FalseSymbol;
        }
    };
    
    template<int i,class R,class...Args> std::function<R(Args...)> is_void_return<i,R,Args...>::theFunc;
    template<int i,class...Args> std::function<void(Args...)> is_void_return<i,void,Args...>::theFunc;
    
    template<int i, class R, class...Args>
    void define_function(void*theEnv,const char*name, std::function<R(Args...)>&&theFunc)
    {
        char argumentsCode[4+9] = {'0','0','u','\0'};
        build_arguments_code<R, Args...>::work(argumentsCode, 3);
        typedef is_void_return<i,R,Args...> _;
        _::theFunc = theFunc;
        EnvDefineFunction2(theEnv, (char*)name, 'u', PTIEF _::f, (char*)name, argumentsCode);
    };
    
    template<int i,class R,class...Args>
    void define_function(void*theEnv,const char*name, R(*theFunc)(Args...))
    {
        define_function<i>(theEnv, name, std::function<R(Args...)>(theFunc));
    };
    
//    template<int i, class R, class...Args>
//    void define_function(void*theEnv,const char*name, std::function<R(Args...)>theFunc)
//    {
//        define_function<i,R,Args...>(theEnv, name, function<R,Args...>(theFunc));
//    };

    //////////////////////////////////////////////////////////////////////////////////////////
}//namespace clips {

