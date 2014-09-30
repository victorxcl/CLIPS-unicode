#include "setup.h"
#include <stdio.h>
#include <ctype.h>
int isalpha_unicode(int inchar)
{
	return isalpha(inchar) || (!isascii(inchar) && EOF != inchar);
}

int isprint_unicode(int inchar)
{
	return isprint(inchar) || (!isascii(inchar) && EOF != inchar);
}

int isalnum_unicode(int inchar)
{
	return isalnum(inchar) || (!isascii(inchar) && EOF != inchar);
}