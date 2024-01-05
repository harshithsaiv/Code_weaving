# Code_weaving
This repository contains an SML (Standard ML) program designed to calculate the sum of a specific series.The series follows the pattern 0^0 + 1^1 + 2^2 + ... + n^n, where 0^0 is considered as 1. The program is configured for n = 6, resulting in an expected sum of 3414.
Abstract Syntax
Types and Declarations (TypeDeclarations.sml)

    Boolean_constant: Represents boolean constants.
    Integer_constant: Represents integer constants.
    variables: Represents variables.
    arithmetic: Represents arithmetic operators (Plus, Minus, Times, Div).
    relational: Represents relational operators (Eq, Le, Lt, Ne, Ge, Gt).
    boolean: Represents boolean operators (And, Or).
    operator: Represents a combination of operators with expressions.
    expression: Represents expressions involving variables, constants, and operators.
    instruction: Represents program instructions, including assignments, conditionals, loops, and sequences.
    Type: Represents data types (Integer, Boolean).
    declaration: Represents variable declarations.
    declarationList: Represents a list of variable declarations.
    program: Represents the overall structure of the program, including a declaration list and the main set of instructions.

Variables and Declarations (SumCalculation.sml)

The program declares and initializes variables representing components of the sum calculation, such as sum, n, i, j, current, and result. These variables have corresponding types specified in the declaration section. A declaration list (decList) collects all these declarations.
Expressions and Instructions (SumCalculation.sml)

The program constructs expressions and instructions to implement the logic of sum calculation. Expressions involve arithmetic and boolean operations on variables, constants, and operators. Instructions include assignments, conditionals (if-else), and loops (while). The overall set of instructions forms the program logic.
Static Semantics
Declaration List Validity (TypeDeclarations.sml)

The program checks the validity of the declaration list. It ensures that variables are not duplicated in the list by defining the NoShowIDeclist function. The DecListValidity function checks if the declaration list is valid, ensuring no variable is repeated.
Type Mapping Table (TypeDeclarations.sml)

The program introduces a type mapping table (TypeMapTable) to associate variables with their internal types. The ChangeTypeMapTable function allows updating this table based on variable declarations. The DecListToTypeMapTable function builds the type mapping table from the declaration list.
Expression and Instruction Validity (ValidationTests.sml)

Functions such as TypeOfExpression and ExpressionValidity are defined to determine the internal types of expressions and check their validity, respectively. InstructionValidity checks the static validity of program instructions. These functions contribute to the overall static semantics of the program.
Dynamic Semantics
Program Validity (ValidationTests.sml)

The ProgramValidity function combines the static checks to ensure the overall validity of the program. It checks both the declaration list and the program body. Exceptions (InvalidDecList and InvalidProgramBody) are raised if the program fails these checks.
Running the Program (SumCalculation.sml)

The main program, Sum.calculateSum(), executes the dynamic semantics of the program. It involves the initialization of variables, computation of expressions, and the execution of instructions within loops and conditionals. The dynamic semantics govern how the program behaves at runtime, leading to the final calculated sum.
How to Run

    Install an SML interpreter/compiler:
        You can use SML/NJ or any other SML interpreter/compiler of your choice.

    Open SML REPL:
        Open a terminal and start the SML REPL.

    Load Program:
        In the SML REPL, type use "SumCalculation.sml"; to load the main program.

    Execute Program:
        Run the program by calling the main function, e.g., Sum.calculateSum();.

Testing

    Load Tests:
        In the SML REPL, type use "ValidationTests.sml"; to load the validation tests.

    Run Tests:
        Execute the tests by calling the corresponding test functions, e.g., ValidationTests.testDeclarationValidity();. This helps ensure the correctness of different aspects of the program.

Exceptions

The program may raise two exceptions:

    InvalidDecList:
        This exception is raised if the provided declaration list is invalid. Check the declaration list for correctness.

    InvalidProgramBody:
        Raised if there are issues with the program's body. This typically indicates a problem with the logic or structure of the program.

Contributions

Contributions are welcome! Feel free to open issues for bugs or suggest enhancements. If you'd like to contribute code, fork the repository and submit a pull request.
License

This program is provided under the MIT License. See the LICENSE file for details.
