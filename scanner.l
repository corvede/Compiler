%{
#include <stdlib.h>
#include <string.h>
#define LIST				strcat(buf, yytext)
#define token(t)			{LIST; if (Opt_T) printf("<%s>\n", #t);}
#define tokenChar(t)		{LIST; if (Opt_T) printf("<%c>\n", (t));}
#define tokenInteger(t, i)	{LIST; if (Opt_T) printf("<%s: %d>\n", #t, (i));}
#define tokenString(t, s)	{LIST; if (Opt_T) printf("<%s: %s>\n", #t, (s));}
#define MAX_LINE_LENG		256

int Opt_S = 1;
int Opt_T = 1;
int linenum = 1;
char yystring[MAX_LINE_LENG];
char buf[MAX_LINE_LENG];
char *removeDoubleQuote(char *in);
char *out;
%}

digit			[0-9]
natural			[1-9][0-9]*
nonnegative		0|{natural}
octal			0[0-7]+
decimal			{nonnegative}
fraction		0|[0-9]*
float			{nonnegative}\.{fraction}
scientific		({nonnegative}|{octal}|{float})[eE][+-]?({nonnegative}|{octal})
letter			[a-zA-Z]
id				{letter}({letter}|{digit})*
endS			["\""/^"\""]
string			"\"".*"\""
spaces			[ \t]+
%x STRING COMMENT

%%
	/* Keywords */
"array"			{token(KWarray);}
"begin"			{token(KWbegin);}
"boolean"		{token(KWboolean);}
"def"			{token(KWdef);}
"do"			{token(KWdo);}
"else"			{token(KWelse);}
"end"			{token(KWend);}
"false"			{token(KWfalse);}
"for"			{token(KWfor);}
"integer"		{token(KWinteger);}
"if"			{token(KWif);}
"of"			{token(KWof);}
"print"			{token(KWprint);}
"read"			{token(KWread);}
"real"			{token(KWreal);}
"string"		{token(KWstring);}
"then"			{token(KWthen);}
"to"			{token(KWto);}
"true"			{token(KWtrue);}
"return"		{token(KWreturn);}
"var"			{token(KWvar);}
"while"			{token(KWend);}
	/* Arithmetic, Relational, and Logical Operators */
"+"				{token(+);}
"-"				{token(-);}
"*"				{token(*);}
"/"				{token(/);}
"mod"			{token(mod);}
":="			{token(:=);}
"<"				{token(<);}
"<="			{token(<=);}
"<>"			{token(<>);}
">="			{token(>=);}
">"				{token(>);}
"="				{token(=);}
"and"			{token(and);}
"or"			{token(or);}
"not"			{token(not);}
	/* Delimiters */
","				{tokenChar(yytext);}
";"				{token(;);}
":"				{token(:);}
"("				{tokenChar('(');}
")"				{tokenChar(')');}
"["				{token([);}
"]"				{token(]);}
	/* Integer Constants */
{decimal}		{tokenInteger(integer, atoi(yytext));}
{octal}			{tokenString(oct_integer, yytext);}
	/* String Constants */
"\""			{LIST; yystring[0] = '\0'; BEGIN STRING;}
<STRING>"\"\""	{LIST; strcat(yystring, "\"");}
<STRING>{endS}	{tokenString(string, yystring); BEGIN INITIAL;}
<STRING>.		{LIST; strcat(yystring, yytext);}
	/* Floating point Constants */
{float}			{tokenString(float, yytext);}
	/* Scientific Notat
	ions */
{scientific}	{tokenString(scientific, yytext);}
	/* Identifiers */
{id}			{tokenString(id, yytext);}
	/* Pseudocomments */
"//&S+".*		{LIST; Opt_S = 1;}
"//&S-".*		{LIST; Opt_S = 0;}
"//&T+".*		{LIST; Opt_T = 1;}
"//&T-".*		{LIST; Opt_T = 0;}
	/* Whitespace */
{spaces}		{LIST;}
\n      {
          LIST;
          if(Opt_S)
			printf("%d: %s", linenum, buf);
          linenum++;
          buf[0] = '\0';
        }
	/* Comment */
"/*"			{LIST; BEGIN COMMENT;}
<COMMENT>.		{LIST;}
<COMMENT>"\n"	{
					LIST;
					if(Opt_S)
						printf("%d: %s", linenum, buf);
					linenum++;
					buf[0] = '\0';
				}
<COMMENT>"*/"	{LIST; BEGIN INITIAL;}
"//".*			{LIST;}
.		{
		printf("error at line %d: bad character \"%s\"\n", linenum, yytext);
		exit(-1);
		}

%%