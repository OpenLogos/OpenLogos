/*
This file is part of OpenLogos/LogOSMaTrans.  Copyright (C) 2005 Globalware AG

OpenLogos/LogOSMaTrans has two licensing options:

The Commercial License, which allows you to provide commercial software
licenses to your customers or distribute Logos MT based applications or to use
LogOSMaTran for commercial purposes. This is for organizations who do not want
to comply with the GNU General Public License (GPL) in releasing the source
code for their applications as open source / free software.

The Open Source License allows you to offer your software under an open source
/ free software license to all who wish to use, modify, and distribute it
freely. The Open Source License allows you to use the software at no charge
under the condition that if you use OpenLogos/LogOSMaTran in an application you
redistribute, the complete source code for your application must be available
and freely redistributable under reasonable conditions. GlobalWare AG bases its
interpretation of the GPL on the Free Software Foundation's Frequently Asked
Questions.

OpenLogos is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the License conditions along with this
program. If not, write to Globalware AG, Hospitalstraﬂe 6, D-99817 Eisenach.

Linux port modifications and additions by Bernd Kiefer, Walter Kasper,
Deutsches Forschungszentrum fuer kuenstliche Intelligenz (DFKI)
Stuhlsatzenhausweg 3, D-66123 Saarbruecken
*/
/*
STREVAL:  A function to evaluate a possibly parenthetized logical expression.
Author:   Ashwin Kalbag
Created:  10/20/93
*/

#include <stdio.h>
/* dan comment out hope not used #include "fortrancall.h"*/
/*
#define	DEBUG
*/

extern void* mh_alloc(unsigned int szRequested);
extern void mh_free(void *pMemory);

#define	MSG1	"\nSTREVAL:  Ill-formed expression, two consecutive expressions without infix operator\n"
#define	MSG2	"\nSTREVAL:  Ill-formed expression, %c operator with no expression to its left\n"
#define	MSG3	"\nSTREVAL:  Ill-formed expression, %c operator with no expression to its right\n"
#define	MSG4	"\nSTREVAL:  Ill-formed expression, unmatched left paren in tagset\n"
#define	MSG5	"\nSTREVAL:  Ill-formed expression, unmatched right paren in tagset\n"
#define	MSG6	"\nSTREVAL:  Unrecognized character in tagset string, cannot evaluate tagset\n"
#define	MSG7	"\nSTREVAL:  Parenthetized null expression found, cannot evaluate tagset\n"
#define	MSG8	"\nSTREVAL:  Null expression found, cannot evaluate tagset\n"
#define	MSG9	"\nSTREVAL:  streval internal error, root of tree is null!!\n"
#define	MSG10	"\nSTREVAL:  streval internal error, root of tree points to garbage!!\n"

#define	OR	'|'
#define	AND	'&'
#define	ZERO	'0'
#define	ONE	'1'
#define	LPAREN	'('
#define	RPAREN	')'
#define	BLANK	' '

typedef struct node { char ch; struct node *left, *right, *parent; } TreeNode;

/*-----------------------------------------------------------------------------------------
New:	Gets a new TreeNode from heap and initializes it to given values.
	Value returned:  Pointer to new TreeNode
-----------------------------------------------------------------------------------------*/
static TreeNode *New(ch, left, right, parent)
char ch;
TreeNode *left, *right, *parent;
{
TreeNode *tmp;
if (tmp = (TreeNode *) mh_alloc(sizeof(TreeNode)))
   { tmp->ch = ch, tmp->left = left, tmp->right = right, tmp->parent = parent; }
return tmp;
}

/*-----------------------------------------------------------------------------------------
Write:	Writes out the tree structure starting at the current node.
	Value returned:  None
-----------------------------------------------------------------------------------------*/
static void Write(current, level)
TreeNode *current;
int level;
{
if (current) {
   printf("level = %d, char = %c\n", level, current->ch);
   Write(current->left, level+1);
   Write(current->right, level+1);
   }
}

/*-----------------------------------------------------------------------------------------
Free:	Frees all nodes of the subtree starting at current node.
	Value returned:  None
-----------------------------------------------------------------------------------------*/
static void Free(current)
TreeNode *current;
{
if (current->left)	Free(current->left);
if (current->right)	Free(current->right);
mh_free(current);
}

/*-----------------------------------------------------------------------------------------
Insert:	Makes the subtree pointed to by location (where object is to be inserted)
	the left subtree of the object to insert.  Other links are preserved.
	Value returned:  1 success/0 failure
-----------------------------------------------------------------------------------------*/
static int Insert(object, location)
TreeNode *object, *location;
{
if (!location) return 0;
else {
   if (location == location->parent)	object->parent = object;
   else					object->parent = location->parent;
   object->left = location;
   if (location != location->parent) {
      if (location->parent->left == location)	location->parent->left = object;
      else					location->parent->right = object;
      }
   location->parent = object;
   }
return 1;
}

/*-----------------------------------------------------------------------------------------
Evaluate:	Evaluates the subtree starting at current node.
		Value returned:  1/0 - result of evaluation
-----------------------------------------------------------------------------------------*/
static int Evaluate(current)
TreeNode *current;
{
if (current->ch == ZERO || current->ch == ONE)	return (current->ch - ZERO);
else if (current->ch == LPAREN)		return Evaluate(current->left);
else if (current->ch == AND)		return Evaluate(current->left) && Evaluate(current->right);
else if (current->ch == OR)		return Evaluate(current->left) || Evaluate(current->right);
}

/*-----------------------------------------------------------------------------------------
STREVAL:	Evaluates a parenthetized logical expression.
		Value returned:  1(failure)/0(success) = result of evaluation,
				-1 if ill-formed expression
-----------------------------------------------------------------------------------------*/
short streval(char *string)
{
   int result;
   register char *stringEnd = string;
   TreeNode *current = NULL, *root = NULL, *tmp;

   /* Make sure the parameter is not null and contains a non-null string to be evaluated */
   if (!string || !*string) { fprintf(stderr, MSG8); return (short) -1; }

   while (*stringEnd != BLANK) stringEnd++;	/* Compute where the string terminates */

   while (string < stringEnd)			/* Loop until string is over */
      {
      switch (*string)
         {
	 case LPAREN:
	 case ZERO:
	 case ONE:
		if (!current)
		   current = New(*string, NULL, NULL, NULL), current->parent = root = current;
		else if (current->ch == ZERO || current->ch == ONE || current->left && current->right)
		   { fprintf(stderr, MSG1); Write(root, 0); return -1; }
		else if (!current->left) {
		   current->left = New(*string, NULL, NULL, current);
		   if (current->ch == LPAREN) current = current->left;
		   }
		else if (!current->right) {
		   current->right = New(*string, NULL, NULL, current);
		   if (*string == LPAREN)	current = current->right;
		   else ; /* Allow "current" to point to the AND/OR node even if subtree is full */
		   }
		break;
	 case RPAREN:
		if (!current || current == root && current->left && current->right)
		   { fprintf(stderr, MSG5); Write(root, 0); return -1; }
		else if (current->ch == LPAREN && !current->left)
		   { fprintf(stderr, MSG7); Write(root, 0); return -1; }
		else if ((current->ch == AND || current->ch == OR) && !current->right)
		   { fprintf(stderr, MSG3, current->ch); Write(root, 0); return -1; }
		else {
		   while (current != root)	/* Loop to set current to highest filled subtree */
		      if (current->parent->right)	/* If current node's parent's subtree is filled */
			 current = current->parent;	/* Set current node to its own parent */
		      else break;			/* If parent's subtree NOT filled, break loop */
		   current->parent->right = New(*string, NULL, NULL, current->parent);
		   while (current != root)	/* Loop to set current to highest filled subtree */
		      if (current->parent->right)	/* If current node's parent's subtree is filled */
			 current = current->parent;	/* Set current node to its own parent */
		      else break;			/* If parent's subtree NOT filled, break loop */
		   }
		break;
	 case AND:
	 case OR:
		if (!current || current->ch == LPAREN && !current->left)
		   { fprintf(stderr, MSG2, *string); Write(root, 0); return -1; }
		else if (current->ch != AND && current->ch != RPAREN) {	/* i.e., if OR/ZERO/ONE/LPAREN */
		   if (!current->right) {	/* Check if unfilled subtree before inserting operator */
		      if (current->ch == LPAREN)
			 { fprintf(stderr, MSG4); Write(root, 0); return -1; }
		      else if (current->ch == OR)
			 { fprintf(stderr, MSG3, OR); Write(root, 0); return -1; }
		      }
		   tmp = New(*string, NULL, NULL, NULL), Insert(tmp, current);
		   if (current == root) root = tmp;
		   current = tmp;
		   }
		else if (current->ch == AND) {
		   if (!current->right)
		      { fprintf(stderr, MSG3, AND); Write(root, 0); return -1; }
		   tmp = New(*string, NULL, NULL, NULL);
		   if (*string == OR)	Insert(tmp, current->right); /* OR after AND is its right child */
		   else {
		      Insert(tmp, current);		/* If inserting tmp into current position, */
		      if (current == root) root = tmp;	/* if current was root, update root to tmp */
		      }
		   current = tmp;
		   }
		break;
	 default:
		fprintf(stderr, MSG6); Write(root, 0); return -1;
         }	/* switch */
      string++;		/* Advance to next character in string */
      }		/* while */

   if (!root) { fprintf(stderr, MSG9); return -1; }	/* Check that root points to something */

   switch (root->ch) /* Check for incomplete expressions, they cannot all be caught while building tree */
      {
      case LPAREN: if (!root->right) { fprintf(stderr, MSG4); Write(root, 0); return -1; }
	           break;
      case AND:
      case OR:     if (!root->left) { fprintf(stderr, MSG2, root->ch); Write(root, 0); return -1; }
		   if (!root->right) { fprintf(stderr, MSG3, root->ch); Write(root, 0); return -1; }
		   break;
      case ZERO:
      case ONE:    break;
      default:     { fprintf(stderr, MSG10); return -1; }
		   break;
      }

   result = Evaluate(root);	/* The tree passed error checks, now evaluate the tree */

#ifdef DEBUG
   printf("\n<< Tree Structure >>\n");
   Write(root, 0);
   printf("\n");
#endif

   Free(root);						/* Free tree nodes */
   return (result == 0) ? (short) 1 : (short) 0;	/* Invert result from C truth value to Logos */
}
