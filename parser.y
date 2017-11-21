%{
#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
%}

/* Delimiters */
%token SEMICOLON
%token COLON
%token COMMA
%token '(' ')'
%token '[' '['
	/* Keywords */
%token END
%token OF
%token BEG
%token PRINT
%token READ
%token RETURN
%token VAR
%token TO
	/* Operators */
%left '+' '-'
%left '*' '/' '%'
%left NOT
%left OR
%left AND
%nonassoc LT LE NE GT GE EQ
%token ASSIGN
	/* Constant Number */
%token BOOL_CONST
%token INT_CONST
%token STR_CONST
%token REAL_CONST
	/* Data Types */
%token BOOL
%token INT
%token STR
%token REAL
%token ARRAY
	/* Conditional */
%token IF
%token ELSE
%token THEN
	/* Loop */
%token FOR
%token WHILE
%token DO
	/* Identifiers */
%token IDENT

%%
	/* Program */
program			: programname SEMICOLON programbody END identifier
		;

programname		: identifier
		;
		
programbody		: data_declare func_declare1 comp_stmt
		;
		
	/* Data declaration */
data_declare	: var_declare data_declare | const_declare data_declare |
		;
		
	/* Var declaration */
var_declare		: VAR id_list COLON scalar_type SEMICOLON | VAR id_list COLON struct_type SEMICOLON
		;
		
	/* Constant declaration */
const_declare	: VAR id_list COLON liter_const SEMICOLON
		;
		
	/* Function declaration */
func_declare1	: func_declare1 func_declare2 | /* no func_declare */
		;
		
func_declare2	: identifier '(' arg_declare ')' COLON type SEMICOLON comp_stmt END identifier | proc_declare /* procedure */
		;
		
proc_declare	: identifier '(' arg_declare ')' SEMICOLON comp_stmt END identifier
		;
		
arg_declare		: declare_list | /* no argument */
		;
		
declare_list	: declaration SEMICOLON declare_list | declaration /* single declaration */
		;
		
declaration		: id_list COLON type
		;
		
	/* Identifiers */
identifier		: IDENT
		;
		
id_list			: identifier COMMA id_list | identifier /* single identifier */
		;
		
	/* Data types */
type		: scalar_type | struct_type
		;
		
scalar_type	: BOOL | INT | REAL | STR
		;
		
struct_type : ARRAY INT_CONST TO INT_CONST OF type
		;
		
liter_const	: BOOL_CONST | INT_CONST | REAL_CONST | STR_CONST
		;
		
	/* Function invocation */
func_invoc	: identifier '(' expr_args ')'
		;
		
expr_args	: expr_list | /* no arguments */
		;
		
expr_list	: expression COMMA expr_list | expression
		;
		
	/* Statements */
statements	: comp_stmt statements | simp_stmt statements | cond_stmt statements | while_stmt statements
			| for_stmt statements | return_stmt statements | func_invoc SEMICOLON statements |
		;
		
comp_stmt	: BEG data_decls statements END
		;
		
simp_stmt	: var_ref ASSIGN expression SEMICOLON | PRINT var_ref SEMICOLON | PRINT expression SEMICOLON | READ var_ref SEMICOLON
		;
		
cond_stmt	: IF bool_expr THEN statements ELSE statements END IF
			| IF bool_expr THEN statements END IF
		;
		
while_stmt	: WHILE bool_expr DO statements END DO
		;
		
for_stmt	: FOR identifier ASSIGN INT_CONST TO INT_CONST DO statements END DO
		;
		
return_stmt	: RETURN expression SEMICOLON
		;
		
var_ref		: identifier
			| identifier arr_indices
		;
		
arr_indices	: '[' int_expr ']' arr_indices |
		;
		
	/* Expressions */
bool_expr	: expression
		;
		
int_expr	: expression
		;
		
expression	: expression '+' expression | expression '-' expression | expression '*' expression | expression '/' expression
			| expression '%' expression | '-' expression %prec '*' | '(' expression ')' | expression LT expression
			| expression LE expression | expression NE expression | expression GT expression | expression GE expression
			| expression EQ expression | NOT expression  | expression AND expression | expression OR expression
			| liter_const | func_invoc | var_ref
		;
%%

int yyerror( char *msg )
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}

