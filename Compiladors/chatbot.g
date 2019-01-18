#header
<<
#include <vector>
#include <map>
#include <string>
#include <iostream>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
  int type;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text; int type;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
// --OLD-- #define createASTlist #0=new AST;(#0)->kind="list";(#0)->right=NULL;(#0)->down=_sibling;
>>

<<
#include <cstdlib>
#include <cmath>

// global variable map to keep track of variables 
map<string,string> m;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
    attr->kind = text;
    attr->text = "";
    attr->type = type;
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->type = attr->type;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

// create a new AST with list as node 
AST* createASTlist(AST *child) {
  AST *as=new AST;
  as->kind="list";
  as->right=NULL;
  as->down=child;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
 AST *c=a->down;
 for (int i=0; c!=NULL && i<n; i++) c=c->right;
 return c;
} 

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

void initialize_seed(AST *a) {
    
    //We point to the seed in the AST
    AST *aux = a->right->right->down; 
    const char* c_seed = (aux->kind).c_str();
    int seed = atoi(c_seed);
    srand(seed);
    
    // Print header
    cout << endl; // To give some space with the AST just above
    cout << "#################################################" << endl;
    cout << "#             Welcome to the ChatBot            #" << endl;
    cout << "#         Seed for this execution is " << seed << "         #" << endl;
    cout << "#################################################" << endl;
    cout << endl; // To give some space with interaction below
}

void store_information(AST* a) {
    
    // While there are Q/A/C still not stored
    while (a != NULL) {

        // ID of the Q/A/C
        string id = a->kind;

        // Case CONVERSATION
        if (a->down->type == ARROW) {
            // We build the string to be stored for this C
            // Arbitrary separator ',' between linked Q & A
            // Example C1:PC->RC becomes <"C1" , "PC,RC"> in map
            string conversation =   a->down->down->kind + "," + a->down->down->right->kind;
            m[id] = conversation;
        }


        // Case ANSWER
        // Every answer has at least 1 option
        else if (a->down->down->type == NUM) {

            // Pointer to first option
            AST* aux = a->down->down; 
            // We build the string to be stored
            string answer = ""; 

            while (aux != NULL) {

                answer = answer + aux->kind + ":"; // NUM:
                // Pointer to the first word of current option
                AST* aux2 = aux->down->down;
                
                while(aux2 != NULL) {
                    
                    answer = answer + " " + aux2->kind; // " "WORD
                    aux2 = aux2->right;
                }

                //We add a newline after each option
                answer += "\n";
                // Move to next option in answer
                aux = aux->right;
            }
            // Store the well formatted answer string, 
            // ready to be printed when needed
            m[id] = answer;
        }

        // Case QUESTION
        else {
            
            // Pointer to first word of Q
            AST* aux = a->down->down;
            // We build the string question
            string question = "";
            while(aux != NULL) {

                question = question + " " + aux->kind;
                aux = aux->right;
            }
            // We add the question mark ?
            question += " ?";
            // Store well formatted question
            m[id] = question;
        }

        // We move to the next Q/A/C
        a = a->right;
    }
}


void execute_conversation(const string& chatbot_id, const string& person_name, string conv) {
    
    // First we parse the conv string to get the IDs of Q and A separately
    // Our format is "ID_Q,ID_A"
    int comma_position = conv.find(',');
    string id_question = conv.substr(0,comma_position);
    string id_answer = conv.substr(comma_position+1,conv.size());
    
    // Now we execute the conversation
    cout << chatbot_id << " > " << person_name << "," << m[id_question] << endl;
    cout << m[id_answer]; // Answer comes with newline at the end of each option so no endl needed
    cout << person_name << " > ";
    string person_answer;
    cin \>\> person_answer;
}

void execute_chatbot_recursive(const string& chatbot_id, const string& person_name, AST* cr) {
    
    // SEQUENCE of executions, first child of THEN, second child of THEN
    if (cr->type == THEN) {
        execute_chatbot_recursive(chatbot_id, person_name, child(cr,0));
        execute_chatbot_recursive(chatbot_id, person_name, child(cr,1));
    }
    
    // DISJUNCTION of executions, just one randomly chosen child of 
    // the OR is executed
    else if (cr->type == OR) {
        
        // We must find how many children the OR has
        AST* aux = cr->down;
        int num_children = 0;
        while(aux != NULL) {
            aux = aux->right;
            ++num_children;
        }

        // Now we just select a RANDOM child from all of them
        int child_to_execute = rand() % num_children;
        execute_chatbot_recursive(chatbot_id, person_name, child(cr,child_to_execute));
    }
    
    // CONVERSATION to execute, we are in front of his id, BASE CASE
    else {
        // Conv to execute in the format "ID_Q,ID_A"
        string conv = m[cr->kind];
        execute_conversation(chatbot_id, person_name, conv);
    }
}

void execute_chatbot(const string& chatbot_id, const string& person_name, AST* c) {
    
    // We search for the chat to be executed in the chats part
    // i.e. the sequence of conversations assigned to chat with chatbot_id
    c = c->down;
    while(c->kind != chatbot_id and c != NULL)
        c = c->right;
    
    // If we cant find it, it means the interaction part contains a chat 
    // that has not been defined 
    if (c == NULL) {
        cout << "No such chat has been defined" << endl;
        return;
    }
    
    // We execute the conversations associated to the chat recursively
    // Were base cases are CONVERSATIONS QUESTION->ANSWER
    execute_chatbot_recursive(chatbot_id, person_name, c->down);
}

void execute_interaction(AST* a, AST* c) {

    // While there are chatbots to execute
    while (a != NULL) {

        string chatbot_id = a->kind;
        string person_name;
        
        // We ask for the user's name
        cout << chatbot_id << " > WHAT IS YOUR NAME ? _" << endl;
        cin \>\> person_name;

        // Execute the current chatbot with id = chatbot_id
        execute_chatbot(chatbot_id, person_name, c);

        // We print final message and jump to the next 
        // chatbot in interaction list
        cout << chatbot_id << " > THANKS FOR THE CHAT " << person_name << "!" << endl << endl;
        a = a->right;
    }
}


int main() {
    
    AST *root = NULL;
    ANTLR(chatbot(&root), stdin);
    ASTPrint(root);
    
    // Initialize the seed for the RNG (srand is global)
    // Also prints a nice welcoming header for the program
    initialize_seed(root->down);

    // Store information about QUESTIONS, ANSWERS & CONVERSATIONS (Q/A/C)
    // ------------ IMPORTANT ------------ //
    // We store them all in the same map m, so we assume that indetifiers
    // are ALL DIFFERENT across QUESTIONS, ANSWERS & CONVERSATIONS
    // We pass a pointer to the first Q/A/C
    store_information(root->down->down);

    // Execute INTERACTION PART now that we have all Q/A/C stored
    // We pass a pointer to the first chatbot to be executed and 
    // a pointer to the parent of all the chats (2nd child of root)
    execute_interaction(root->down->right->right->down->right, root->down->right);
    
    // Uncomment to print ALL the contents of the map
    /**
    for (map<string,string>::const_iterator it = m.begin(); it != m.end(); ++it) {
        cout << it->first << endl << it->second;
    } **/
}
>>


/*********************************** LEXIC **************************************/
#lexclass START

#token OPEN_PAR         "\("
#token CLOSE_PAR        "\)"
#token OPEN_BRACK       "\["
#token CLOSE_BRACK      "\]"

#token COMA             "\,"
#token COLON            "\:"
#token SEMICOLON        "\;"
#token ARROW            "\->"
#token HASHTAG          "\#"
#token INTERROGATION    "\?"

#token INTERACTION      "INTERACTION"
#token END              "END"
#token CHATBOT          "CHATBOT"
#token THEN             "THEN"
#token OR               "OR"
#token CONVERSATION     "CONVERSATION"
#token QUESTION         "QUESTION"
#token ANSWERS          "ANSWERS"
#token NUM              "[0-9]+"
#token ID               "[A-Za-z][A-Za-z0-9]*"

#token SPACE "[\ \n]" << zzskip();>>


/*********************************** GRAMATICA **************************************/
chatbot:                conversations chats startchat <<#0=createASTlist(_sibling);>>;
/*********************************** chats ******************************************/
chats:                  (chat)* <<#0=createASTlist(_sibling);>>;
chat:                   CHATBOT! ID^ COLON! chat_body;

chat_body:              term ( | OR^ term (OR! term)*) ;
term:                   atom (THEN^ atom)* ;
atom:                   HASHTAG! ID | OPEN_PAR! chat_body CLOSE_PAR! ;
/********************************* conversations ************************************/
conversations:          (conversation)* <<#0=createASTlist(_sibling);>>;

conversation:           (ID^ COLON! (question | answer | relation)) ;

question:               (QUESTION! question_body) ;
question_body:          (ID)+ INTERROGATION! <<#0=createASTlist(_sibling);>>;

answer:                 (ANSWERS! answer_body) ;
answer_body:            (answer_list | answer_list_2) <<#0=createASTlist(_sibling);>>;
/******************************* llista format 1 **********************************/
answer_list:            (answer_element)* ;
answer_element:         (NUM^ COLON! answer_element_body) ;
answer_element_body:    ((ID)+ SEMICOLON!) <<#0=createASTlist(_sibling);>>;
/**********************************************************************************/
/******************************* llista format 2 **********************************/
answer_list_2:          OPEN_BRACK! answer_list_2_body CLOSE_BRACK! ; 
answer_list_2_body:     (al2b_element (COMA! al2b_element)*) ;
al2b_element:           (OPEN_PAR! NUM^ COMA! al2b_element_body CLOSE_PAR!) ;
al2b_element_body:      ((ID)+) <<#0=createASTlist(_sibling);>>; 
/**********************************************************************************/
relation:               CONVERSATION! relation_body ;
relation_body:          HASHTAG! ID ARROW^ HASHTAG! ID ;
/*********************************** startchat ************************************/
startchat:              INTERACTION! NUM (ID)* END! <<#0=createASTlist(_sibling);>>;
/**********************************************************************************/
