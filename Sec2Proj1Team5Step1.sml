

(* /*
Sample Program.
The program is used to calculate the sum of 00 + 11 + 22 + 33 + 44 + ...+ nn.
Here we define 00 = 1.
In this sample, define n = 5, and the sum should be 3414.
*/

PROGRAM sum
{
    sum Integer;
    n Integer;
    i Integer;
    j Integer;
    current Integer;
    result Integer;

    n = 6;

    IF (n Eq 0) THEN sum = 1
    ELSE
    {
        i = 1;
        sum = 1;
        WHILE ( i Le n )
        {
            j = 1;
            current = i;
            WHILE ( j Lt i )
            {
                current = current Times i;
                j = j Plus 1;
            }
            sum = sum Plus current;
            i = i Plus 1;
        }
    } 
}

*)

(* Definition of all the datatype and type which are required to represent the given Program  *)
type Boolean_constant= bool;
type Integer_constant= int;
datatype variables = Variable of string;
datatype arithmetic = Plus |
                      Minus |
                      Times |
                      Div;
datatype relational = Eq |
                      Le | 
                      Lt |
                      Ne |
                      Ge |
                      Gt;
datatype boolean=   And |
                    Or;
datatype operator= Ar of arithmetic | 
                   Re of relational |
                   Be of boolean;
datatype expression = VarExp of variables | 
                      ExpIC of Integer_constant |
                      ExpBC of Boolean_constant | 
                      OpExp of operator*expression*expression;
datatype instruction = Skip|
                       Assignment of variables*expression | 
                       Ifthen of expression*instruction*instruction | 
                       WhileStmt of expression*instruction | 
                       Sequence of instruction list;
datatype Type =Integer | Boolean;
type declaration = variables*Type;
type declarationList = declaration list;
type program = declarationList*instruction;

(*Declaring the variables*)
val sum = Variable "sum";
val n = Variable "n";
val i = Variable  "i";
val j = Variable "j";
val current =  Variable"current";
val result = Variable "result";

val dec1:declaration= (sum,Integer);
val dec2 :declaration= (n,Integer );
val dec3:declaration= (i,Integer );
val dec4:declaration= (j, Integer );
val dec5:declaration= (current,Integer );
val dec6:declaration= (result,Integer );

(* Creating a declaration list which consists of all the declared variables  *)
val declist= [dec1,dec2,dec3,dec4,dec5,dec6];
val dec:declarationList=(declist);

(* Implementation of inner while loop *)
val exp1 = OpExp(Ar Times, VarExp current, VarExp i); (* Compute current * i *)
val ass1 = Assignment(current, exp1);                (* Assign the result to current *)
val exp2 = OpExp(Ar Plus, VarExp j, ExpIC 1);         (* Compute j + 1 *)
val ass2 = Assignment(j, exp2);                     (* Assign the result to j *)
val seq_while1 = [ass1, ass2];                      (* Create a sequence of instructions *)
val seq_while11 = Sequence(seq_while1);             (* Create a sequence for the inner while loop *)
val while_cond = OpExp(Re Lt, VarExp j, VarExp i);   (* Check if j < i *)
val while1 = WhileStmt(while_cond, seq_while11);    (* Create the inner while loop *)

(* Implementation of the outer while loop *)
val ass3 = Assignment(j, ExpIC 1);                    (* Initialize j to 1 *)
val ass4 = Assignment(current, VarExp i);           (* Initialize current to i *)
val exp3 = OpExp(Ar Plus, VarExp sum, VarExp current); (* Compute sum + current *)
val ass5 = Assignment(sum, exp3);                   (* Assign the result to sum *)
val exp4 = OpExp(Ar Plus, VarExp i, ExpIC 1);         (* Compute i + 1 *)
val ass6 = Assignment(i, exp4);                     (* Assign the result to i *)
val seq_while2 = [ass3, ass4, while1, ass5, ass6];   (* Create a sequence of instructions *)
val seq_while22 = Sequence(seq_while2);             (* Create a sequence for the outer while loop *)
val while2_cond = OpExp(Re Le, VarExp i, VarExp n); (* Check if i <= n *)
val while2 = WhileStmt(while2_cond, seq_while22);   (* Create the outer while loop *)

(*Implimentation of Else*)           
val ass7 = Assignment(i, ExpIC 1);                    (* Reset i to 1 *)
val ass8 = Assignment(sum, ExpIC 1);                  (* Reset sum to 1 *)
val seq_else = [ass7, ass8, while2];                (* Create a sequence of instructions for "else" *)
val else_cond = Sequence(seq_else);                 (* Create the "else" condition *)

(*Implimentation of If*)
val ass9 = Assignment(sum, ExpIC 1);                  (* Set sum to 1 *)
val if_condition = OpExp(Re Eq, VarExp n, ExpIC 0);   (* Check if n is equal to 0 *)
val if1 = Ifthen(if_condition, ass9, else_cond);     (* Create the "if" statement *)

(*creating instruction list*)
val ass10 = Assignment(n, ExpIC 6);                   (* Set n to 6 *)
val prg_list = Sequence([ass10, if1]);              (* Create a sequence of instructions *)

(*Combining instruction and declaration list*)
val Sum:program=(dec, prg_list);                 (* Create a program with declarations and instructions *)

(* Step 2: Static Semantics  *)

(* 1. NOShowInDeclist: DeclarationList->Variable->Bool *)
val rec NoShowIDeclist=
(
    fn ([])=>
    (
        fn(z:variables)=> true 
    ) | 
        ((x:variables,y:Type)::TailofList)=>
        (  fn (z:variables)=>
           (    
                (z <> x) andalso NoShowIDeclist(TailofList)(z) 
           )
        )
        
)

(* 2. DecListValidity:DeclrationList->Bool *)
val rec DecListValidity = 
( 
    fn ([])=> true |
       (((x,y)::TailofList):declarationList)=>
       (
            DecListValidity(TailofList) andalso NoShowIDeclist(TailofList)(x)
       )
        
)
(* Testing for the Validity of the Declaration List  *)
(* Testing 1 : Checking whether the Sample program declarartion list is valid or not  *)
(* A Good Testing *)
val valDecListtest1= DecListValidity(declist);

(* Testing 2 : Checking the Declaration List by passing a bad Declaration List *)
(* Bad declaration List *)
(* Bad Test Case *)
val baddec1:declaration= (sum,Integer);
val baddec2 :declaration= (n,Integer );
val baddec3:declaration= (sum,Integer );
val baddec4:declaration= (j, Boolean );
val baddeclist= [baddec1,baddec2,baddec3,baddec4];
val baddec:declarationList=(baddeclist);

val valDecListtest2= DecListValidity(baddec);

(* 3-7: DeclarationList->TypeMapTable *)

(* 3 *)
datatype  InternalType =InternalNoType | InternalTypeInt | InternalTypeBool;

(* 4 TypeMaptable is a funtion type*)
type TypeMapTable =variables->InternalType;

(* 5 FirstTypeMapTable: TypeMapTable  *)
val FirstTypeMapTable =
(
    fn(v:variables)=>InternalNoType
)

(* Testing for the FirstTypeMapTable with all the variables present in the sample program *)
val test_varaible1= FirstTypeMapTable(sum);
val test_variable2= FirstTypeMapTable(n);
val test_variable3= FirstTypeMapTable(i);
val test_variable4= FirstTypeMapTable(j);
val test_variable5= FirstTypeMapTable(current);
val test_variable6= FirstTypeMapTable(result);

(* 6. ChangeTypeMapTable: TypeMapTable->Declaration->TypeMapTable *)

val ChangeTypeMapTable = 
(
    fn(X:TypeMapTable)=>
    (
        fn (a:variables,Boolean)=>
        (   fn(y:variables)=>
            (   if y=a 
                    then InternalTypeBool 
                else 
                    X(y)
            )
        ) |
           (c:variables,Integer)=>
        (   fn(m:variables)=>
            (   if m=c 
                    then InternalTypeInt 
                else 
                    X(m)
            )
        )
    )
)

(* Testing for ChangeTypeMapTable *)
val MyMapTable1=ChangeTypeMapTable(FirstTypeMapTable)(dec1);
MyMapTable1(sum);
MyMapTable1(n);
MyMapTable1(i);
MyMapTable1(j);
MyMapTable1(current);
MyMapTable1(result);



(* 7. DecListToTypeMapTable : DeclarationList->TypeMapTable *)

val rec DecListToTypeMapTable =
(
    fn([])=>FirstTypeMapTable |
      ((HeadOfList::TailofList):declarationList)=>
        (
            ChangeTypeMapTable(DecListToTypeMapTable(TailofList))(HeadOfList)
        )
)

(* Testing for DecListToTypeMapTable to check if the declaration List is correct after changingType MapTable *)
val myTable = DecListToTypeMapTable(dec);
myTable(sum);
myTable(n);
myTable(i);
myTable(j);
myTable(current);
myTable(result);


(* Check validity of Expression *)

(* 8. TypeOfExpression: TypeMapTable -> Expression-> InternalType *)

val TypeOfExpression =
(
    fn(tmt:TypeMapTable)=>
    (
        fn(VarExp(v))=>tmt(v)|
          (ExpIC(Integer_constant))=>InternalTypeInt |
          (ExpBC(Boolean_constant))=>InternalTypeBool |
          (OpExp(Ar(arithmetic),expp1,expp2))=>InternalTypeInt |
          (OpExp(Be(boolean),expp1,expp2))=>InternalTypeBool |
          (OpExp(Re(relational),expp1,expp2))=>InternalTypeBool
    )
)

(* 9 ExpressionValidity: Expression->TypeMapTable->Bool *)
val rec ExpressionValidity =
(
    fn (VarExp(v))=>
            (
                fn(tmt:TypeMapTable)=>(tmt(v)<> InternalNoType)
            ) |
       (ExpIC(Integer_constant))=>
            (
                fn(tmt:TypeMapTable)=>true 
            ) |
       (ExpBC(Boolean_constant))=>
            (
                fn(tmt:TypeMapTable)=>true 
            ) |
       (OpExp(Ar(arithmetic),expp1,expp2))=>
            (
                fn(tmt:TypeMapTable)=>
                (
                    ExpressionValidity(expp1)(tmt) andalso
                    ExpressionValidity(expp2)(tmt) andalso
                    (TypeOfExpression(tmt)(expp1))=InternalTypeInt andalso
                    (TypeOfExpression(tmt)(expp2))=InternalTypeInt
                )
            ) |
        (OpExp(Be(boolean),expp1,expp2))=>
            (
                fn(tmt:TypeMapTable)=>
                (
                    ExpressionValidity(expp1)(tmt) andalso
                    ExpressionValidity(expp2)(tmt) andalso
                    (TypeOfExpression(tmt)(expp1))=InternalTypeBool andalso
                    (TypeOfExpression(tmt)(expp2))=InternalTypeBool
                )
            ) |
        (OpExp(Re(relational),expp1,expp2))=>
            (
                fn(tmt:TypeMapTable)=>
                (
                    ExpressionValidity(expp1)(tmt) andalso
                    ExpressionValidity(expp2)(tmt) andalso
                    (TypeOfExpression(tmt)(expp1))=InternalTypeInt andalso
                    (TypeOfExpression(tmt)(expp2))=InternalTypeInt
                )
            )    
)

(* Testing for expression *)
(* Good Test Cases taken from Sample Program *)
val case1_exp1=ExpressionValidity(exp1)(myTable)
val case2_exp2=ExpressionValidity(exp2)(myTable)
val case3_exp3=ExpressionValidity(exp3)(myTable)
val case4_exp4=ExpressionValidity(exp4)(myTable)

(* Creating a Good Test Case for Arithmetic Recursive Expression *)
val exp5=OpExp(Ar Plus,exp1,exp2)  (*  (current*i) + (j+1)  *)

(* Creating a Good Test Case for Boolean Recursive Expression *)
val exp6=OpExp(Be And,if_condition,while2_cond)    (* (n==0) and (i<=n) *)
val case5_expr5=ExpressionValidity(exp5)(myTable)
val case6_expr6=ExpressionValidity(exp6)(myTable)

(* Creating a Bad Test Case *)
val exp7= OpExp(Ar Plus,exp1,if_condition) (*Bad condition case*) (* (current*i) + (n==0) we have an arithmetic expression on side and boolean on the other side and we are adding them *)
val case7_expr7=ExpressionValidity(exp7)(myTable)


(* 10. Check for Validity for Instruction  *)
(* InstructionValidity: Instruction -> TypeMapTable->Bool *)
val rec InstructionValidity= 
(
        fn(Skip)=>
        (
            fn(tmt:TypeMapTable)=>true
        ) |
        (Assignment(v,exp))=>
        (
            fn(tmt:TypeMapTable)=>
            (
                (tmt(v)=TypeOfExpression(tmt)(exp)) andalso
                (tmt(v)<>InternalNoType) andalso
                ExpressionValidity(exp)(tmt)
            )
        ) |
        (Ifthen(exp,instt1,instt2))=>
        (
            fn(tmt:TypeMapTable)=>
            (
                (TypeOfExpression(tmt)(exp)=InternalTypeBool) andalso 
                ExpressionValidity(exp)(tmt) andalso
                InstructionValidity(instt1)(tmt)  andalso
                InstructionValidity(instt2)(tmt)
            )
        ) |
        (WhileStmt(exp,inst))=>
        (
            fn(tmt:TypeMapTable)=>
            (
                (TypeOfExpression(tmt)(exp)= InternalTypeBool) andalso
                ExpressionValidity(exp)(tmt) andalso
                InstructionValidity(inst)(tmt)
            )
        ) |
        (Sequence([]))=>
        (
            fn(tmt:TypeMapTable)=>true
        ) |
        (Sequence(HeadOfList::TailofList))=>
        (
            fn(tmt:TypeMapTable)=>
            (
                InstructionValidity(HeadOfList)(tmt) andalso
                InstructionValidity(Sequence(TailofList))(tmt)
            )
        )
)
(* Testing *)
(* Good Testing Cases that have been taken from the sample program *)
val case1_instr=InstructionValidity(ass1)(myTable);  
val case2_instr=InstructionValidity(while1)(myTable);
val case3_instr=InstructionValidity(while2)(myTable); 
val case4_instr=InstructionValidity(if1)(myTable);
val case5_instr=InstructionValidity(prg_list)(myTable); 
val case6_instr=InstructionValidity(ass2)(myTable);

(* Bad Test Case  *)
(* Creating a Bad Test Case specifically Assignment *)
val case7_ass=Assignment(current,ExpBC true); (*Bad case : Doing a wrong Assignment so that the function will give us false*) 
val case7_instr=InstructionValidity(case7_ass)(myTable);


(* 11-13 Check for validity of Program *)

(* 11.  *)
exception InvalidDecList;

(* 12.  *)
exception InvalidProgramBody;

(* 13 ProgramValidity : Program-> bool *)

val ProgramValidity =
(
    fn(decList:declarationList,ProgramBody:instruction)=>
    (
        if DecListValidity(decList) <> true 
            then raise InvalidDecList 
        else
            if InstructionValidity(ProgramBody)(DecListToTypeMapTable(decList)) 
                then true 
            else 
                raise InvalidProgramBody
    )
)
(* Good Test Case  *)
(* Passing our Sample Program int the ProgramValidity Function *)
val res1= ProgramValidity(Sum);

(* Bad Test Case 1: *)
(* Creating a Test Case which has a bad declaration List so that the exception is raised *)
val BadSum:program =(baddec,prg_list);
val re2=ProgramValidity(BadSum);

(* Bad Test Case 2: *)
(* Having a bad Program Body so that the expection InvalidProgramBOdy is raised *)
(* val Badprgrm_list=Sequence([case7_ass,ass10, if1])
val Badprgrm:program =(dec,Badprgrm_list)
val result3=ProgramValidity(Badprgrm) *)

(*  Output of the Second Exception that will be thrown when the Program Body is Wrong
uncaught exception InvalidProgramBody
  raised at: .\Sec2Proj1Team5Step1.sml:366.102-366.120
 *)

