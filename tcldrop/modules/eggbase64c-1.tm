# eggbase64c.tcl --
#
# Base64 Encode/Decode strings Eggdrop style - but with Critcl.
#
# This script provides four different functions that deal with
# Eggdrop's funky implementations of base64.
#
# $Id$
#
# ::eggbase64c::toint <string>
# Converts encoded channel strings used in eggdrop botnet
# protocol back to their corresponding integer values
#
# ::eggbase64c::fromint <string>
# Converts integer channel ids to what eggdrop uses in its botnet
# protocol
#
# ::eggbase64c::encode <blowfishencryptedbinarydata>
# provides eggdrop compatible base64 encoding used in the blowfish
# module. use blowfish.tcl from tcclib to encrypt your data and use
# this proc to encode it
#
# ::eggbase64c::decode <base64encodedstring>
# decodes eggdrop compatible base64 encoded strings back to binary
# data to be then decrypted with blowfish
#
# Pixelz@EFnet 2009-06-20.
#
# Special thanks to thommey for helping out a lot with this, and
# answering all my stupid questions.

package require Tcl 8.5
package require critcl
namespace eval ::eggbase64c {
	variable version 1.0
	variable script [info script]
	regexp -- {^[_[:alpha:]][:_[:alnum:]]*-([[:digit:]].*)[.]tm$} [file tail $script] -> version
	package provide eggbase64c $version
	namespace ensemble create -subcommands [list encode decode toint fromint]
	namespace ensemble create -command ::eggbase64 -subcommands [list encode decode toint fromint]
}


::critcl::ccode {

	static char tobase64array[64] = {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
		'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
		'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'[', ']'
	};

	static char base64to[256] = {
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0, 0,
		0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
		15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 62, 0, 63, 0, 0, 0, 26, 27, 28,
		29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,
		49, 50, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};
	
	#include <string.h>
	#include <stdint.h>
	
	/* Of course, if you change either of these, then your userfile will
	 * no longer be able to be shared. :)
	 */
	#define SALT1 0xdeadd061
	#define SALT2 0x23f6b095

	/* Convert 64-bit encrypted password to text for userfile */
	static char *base64 =
		"./0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

	static int base64dec(char c)
	{
		int i;
		for (i = 0; i < 64; i++)
			if (base64[i] == c)
				return i;
		return 0;
	}
}

namespace eval ::eggbase64c {

	# Thank you Eggdrop :)
	::critcl::cproc encode {char* str} string {
		uint32_t left, right;
		unsigned char *p;
		char *s, *dest, *d;
		int i;
		/* Pad fake string with 8 bytes to make sure there's enough */
		s = malloc(strlen(str) + 9);
		strcpy(s, str);
		p = s;
		dest = malloc((strlen(str) + 9) * 2);
		while (*p)
			p++;
		for (i = 0; i < 8; i++)
			*p++ = 0;
		p = s;
		d = dest;
		while (*p) {
			left = ((*p++) << 24);
			left += ((*p++) << 16);
			left += ((*p++) << 8);
			left += (*p++);
			right = ((*p++) << 24);
			right += ((*p++) << 16);
			right += ((*p++) << 8);
			right += (*p++);
			for (i = 0; i < 6; i++) {
				*d++ = base64[right & 0x3f];
				right = (right >> 6);
			}
			for (i = 0; i < 6; i++) {
				*d++ = base64[left & 0x3f];
				left = (left >> 6);
			}
		}
		*d = 0;
		free(s);
		return dest;
	}

	::critcl::cproc decode {char* str} string {
		uint32_t left, right;
		char *p, *s, *dest, *d;
		int i;
		/* Pad encoded string with 0 bits in case it's bogus */
		s = malloc(strlen(str) + 12);
		strcpy(s, str);
		p = s;
		dest = malloc(strlen(str) + 12);
		while (*p)
			p++;
		for (i = 0; i < 12; i++)
			*p++ = 0;
		p = s;
		d = dest;
		while (*p) {
			right = 0L;
			left = 0L;
			for (i = 0; i < 6; i++)
				right |= (base64dec(*p++)) << (i * 6);
			for (i = 0; i < 6; i++)
				left |= (base64dec(*p++)) << (i * 6);
			for (i = 0; i < 4; i++)
				*d++ = (left & (0xff << ((3 - i) * 8))) >> ((3 - i) * 8);
			for (i = 0; i < 4; i++)
				*d++ = (right & (0xff << ((3 - i) * 8))) >> ((3 - i) * 8);
		}
		*d = 0;
		free(s);
		return dest;
	}

	::critcl::cproc fromint {int val} char* {
		static char buf_base64[12];
		int i = 11;
		buf_base64[11] = 0;
		if (!val) {
			buf_base64[10] = 'A';
			return buf_base64 + 10;
		}
		while (val) {
			i--;
			buf_base64[i] = tobase64array[val & 0x3f];
			val = val >> 6;
		}
		return buf_base64 + i;
	}

	::critcl::cproc toint {char* buf} int {
		int i = 0;
		while (*buf) {
			i = i << 6;
			i += base64to[(int) *buf];
			buf++;
		}
		return i;
	}

}

# pure-tcl eggbase64:
# fromint: 7.8556 microseconds per iteration: Yaf
# toint: 10.6855 microseconds per iteration: 99999
# encode: 56.6081 microseconds per iteration: x.Fxg/JGGEk/wPSWZ0cdqhP0
# decode: 120.8465 microseconds per iteration: V¬-/RÄ¦#¦M+++=ìb

# critcl-powered eggbase64c:
# fromint: 0.7423 microseconds per iteration: Yaf
# toint: 0.5642 microseconds per iteration: 99999
# encode: 1.1445 microseconds per iteration: x.Fxg/JGGEk/wPSWZ0cdqhP0
# decode: 2.4244 microseconds per iteration: V¬-/RÄ¦#¦M+++=ìb
