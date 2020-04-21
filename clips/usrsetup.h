   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  06/28/16             */
   /*                                                     */
   /*               USER SETUP HEADER FILE                */
   /*******************************************************/
#include <boost/predef.h>

#if BOOST_OS_IOS
#   define system(x) EXIT_FAILURE
#endif// BOOST_OS_IOS
