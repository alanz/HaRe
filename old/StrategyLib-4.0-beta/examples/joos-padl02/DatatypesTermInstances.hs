module DatatypesTermInstances where
import Datatypes
import TermRep
{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Datatypes.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Term Assignment where
    explode (x::Assignment) = TermRep (toDyn x, f x, g x) where
	f (Assignment aa ab) = [explode aa,explode ab]
	g (Assignment _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((Assignment (TermRep.fDyn aa) (TermRep.fDyn ab))::Assignment) ; _ -> error "Term explosion error."

_tc_AssignmentTc = mkTyCon "Assignment"
instance Typeable Assignment where
    typeOf x = mkTyConApp _tc_AssignmentTc [ ]

instance Term InstanceCreation where
    explode (x::InstanceCreation) = TermRep (toDyn x, f x, g x) where
	f (InstanceCreation aa ab) = [explode aa,explode ab]
	g (InstanceCreation _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((InstanceCreation (TermRep.fDyn aa) (TermRep.fDyn ab))::InstanceCreation) ; _ -> error "Term explosion error."

_tc_InstanceCreationTc = mkTyCon "InstanceCreation"
instance Typeable InstanceCreation where
    typeOf x = mkTyConApp _tc_InstanceCreationTc [ ]

instance Term MethodInvocation where
    explode (x::MethodInvocation) = TermRep (toDyn x, f x, g x) where
	f (ExpressionInvocation aa ab ac) = [explode aa,explode ab,explode ac]
	f (SuperInvocation ad ae) = [explode ad,explode ae]
	g (ExpressionInvocation _ _ _) xs = case TermRep.fArgs xs of [aa,ab,ac] -> toDyn ((ExpressionInvocation (TermRep.fDyn aa) (TermRep.fDyn ab) (TermRep.fDyn ac))::MethodInvocation) ; _ -> error "Term explosion error."
	g (SuperInvocation _ _) xs = case TermRep.fArgs xs of [ad,ae] -> toDyn ((SuperInvocation (TermRep.fDyn ad) (TermRep.fDyn ae))::MethodInvocation) ; _ -> error "Term explosion error."

_tc_MethodInvocationTc = mkTyCon "MethodInvocation"
instance Typeable MethodInvocation where
    typeOf x = mkTyConApp _tc_MethodInvocationTc [ ]

instance Term Arguments where
    explode (x::Arguments) = TermRep (toDyn x, f x, g x) where
	f (Arguments aa) = [explode aa]
	g (Arguments _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((Arguments (TermRep.fDyn aa))::Arguments) ; _ -> error "Term explosion error."

_tc_ArgumentsTc = mkTyCon "Arguments"
instance Typeable Arguments where
    typeOf x = mkTyConApp _tc_ArgumentsTc [ ]

instance Term Expression where
    explode (x::Expression) = TermRep (toDyn x, f x, g x) where
	f (Literal aa) = [explode aa]
	f (Identifier ab) = [explode ab]
	f This = []
	f (PrefixExpr ac ad) = [explode ac,explode ad]
	f (InfixExpr ae af ag) = [explode ae,explode af,explode ag]
	f (AndOrExpr ah ai aj) = [explode ah,explode ai,explode aj]
	f (InstanceOf ak al) = [explode ak,explode al]
	f (TypeCast am an) = [explode am,explode an]
	f (BracketExpr ao) = [explode ao]
	f (AssignmentExpr ap) = [explode ap]
	f (InstanceCreationExpr aq) = [explode aq]
	f (MethodInvocationExpr ar) = [explode ar]
	g (Literal _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((Literal (TermRep.fDyn aa))::Expression) ; _ -> error "Term explosion error."
	g (Identifier _) xs = case TermRep.fArgs xs of [ab] -> toDyn ((Identifier (TermRep.fDyn ab))::Expression) ; _ -> error "Term explosion error."
	g This xs = case TermRep.fArgs xs of [] -> toDyn ((This)::Expression) ; _ -> error "Term explosion error."
	g (PrefixExpr _ _) xs = case TermRep.fArgs xs of [ac,ad] -> toDyn ((PrefixExpr (TermRep.fDyn ac) (TermRep.fDyn ad))::Expression) ; _ -> error "Term explosion error."
	g (InfixExpr _ _ _) xs = case TermRep.fArgs xs of [ae,af,ag] -> toDyn ((InfixExpr (TermRep.fDyn ae) (TermRep.fDyn af) (TermRep.fDyn ag))::Expression) ; _ -> error "Term explosion error."
	g (AndOrExpr _ _ _) xs = case TermRep.fArgs xs of [ah,ai,aj] -> toDyn ((AndOrExpr (TermRep.fDyn ah) (TermRep.fDyn ai) (TermRep.fDyn aj))::Expression) ; _ -> error "Term explosion error."
	g (InstanceOf _ _) xs = case TermRep.fArgs xs of [ak,al] -> toDyn ((InstanceOf (TermRep.fDyn ak) (TermRep.fDyn al))::Expression) ; _ -> error "Term explosion error."
	g (TypeCast _ _) xs = case TermRep.fArgs xs of [am,an] -> toDyn ((TypeCast (TermRep.fDyn am) (TermRep.fDyn an))::Expression) ; _ -> error "Term explosion error."
	g (BracketExpr _) xs = case TermRep.fArgs xs of [ao] -> toDyn ((BracketExpr (TermRep.fDyn ao))::Expression) ; _ -> error "Term explosion error."
	g (AssignmentExpr _) xs = case TermRep.fArgs xs of [ap] -> toDyn ((AssignmentExpr (TermRep.fDyn ap))::Expression) ; _ -> error "Term explosion error."
	g (InstanceCreationExpr _) xs = case TermRep.fArgs xs of [aq] -> toDyn ((InstanceCreationExpr (TermRep.fDyn aq))::Expression) ; _ -> error "Term explosion error."
	g (MethodInvocationExpr _) xs = case TermRep.fArgs xs of [ar] -> toDyn ((MethodInvocationExpr (TermRep.fDyn ar))::Expression) ; _ -> error "Term explosion error."

_tc_ExpressionTc = mkTyCon "Expression"
instance Typeable Expression where
    typeOf x = mkTyConApp _tc_ExpressionTc [ ]

instance Term AndOr where
    explode (x::AndOr) = TermRep (toDyn x, f x, g x) where
	f AND = []
	f OR = []
	g AND xs = case TermRep.fArgs xs of [] -> toDyn ((AND)::AndOr) ; _ -> error "Term explosion error."
	g OR xs = case TermRep.fArgs xs of [] -> toDyn ((OR)::AndOr) ; _ -> error "Term explosion error."

_tc_AndOrTc = mkTyCon "AndOr"
instance Typeable AndOr where
    typeOf x = mkTyConApp _tc_AndOrTc [ ]

instance Term PrefixOperator where
    explode (x::PrefixOperator) = TermRep (toDyn x, f x, g x) where
	f Neg = []
	f Fac = []
	g Neg xs = case TermRep.fArgs xs of [] -> toDyn ((Neg)::PrefixOperator) ; _ -> error "Term explosion error."
	g Fac xs = case TermRep.fArgs xs of [] -> toDyn ((Fac)::PrefixOperator) ; _ -> error "Term explosion error."

_tc_PrefixOperatorTc = mkTyCon "PrefixOperator"
instance Typeable PrefixOperator where
    typeOf x = mkTyConApp _tc_PrefixOperatorTc [ ]

instance Term InfixOperator where
    explode (x::InfixOperator) = TermRep (toDyn x, f x, g x) where
	f Eq = []
	f NEQ = []
	f Lt = []
	f Gt = []
	f LEQ = []
	f GEQ = []
	f PLUS = []
	f MINUS = []
	f MUL = []
	f DIV = []
	f MOD = []
	g Eq xs = case TermRep.fArgs xs of [] -> toDyn ((Eq)::InfixOperator) ; _ -> error "Term explosion error."
	g NEQ xs = case TermRep.fArgs xs of [] -> toDyn ((NEQ)::InfixOperator) ; _ -> error "Term explosion error."
	g Lt xs = case TermRep.fArgs xs of [] -> toDyn ((Lt)::InfixOperator) ; _ -> error "Term explosion error."
	g Gt xs = case TermRep.fArgs xs of [] -> toDyn ((Gt)::InfixOperator) ; _ -> error "Term explosion error."
	g LEQ xs = case TermRep.fArgs xs of [] -> toDyn ((LEQ)::InfixOperator) ; _ -> error "Term explosion error."
	g GEQ xs = case TermRep.fArgs xs of [] -> toDyn ((GEQ)::InfixOperator) ; _ -> error "Term explosion error."
	g PLUS xs = case TermRep.fArgs xs of [] -> toDyn ((PLUS)::InfixOperator) ; _ -> error "Term explosion error."
	g MINUS xs = case TermRep.fArgs xs of [] -> toDyn ((MINUS)::InfixOperator) ; _ -> error "Term explosion error."
	g MUL xs = case TermRep.fArgs xs of [] -> toDyn ((MUL)::InfixOperator) ; _ -> error "Term explosion error."
	g DIV xs = case TermRep.fArgs xs of [] -> toDyn ((DIV)::InfixOperator) ; _ -> error "Term explosion error."
	g MOD xs = case TermRep.fArgs xs of [] -> toDyn ((MOD)::InfixOperator) ; _ -> error "Term explosion error."

_tc_InfixOperatorTc = mkTyCon "InfixOperator"
instance Typeable InfixOperator where
    typeOf x = mkTyConApp _tc_InfixOperatorTc [ ]

instance Term Literal where
    explode (x::Literal) = TermRep (toDyn x, f x, g x) where
	f (BooleanLit aa) = [explode aa]
	f (IntegerLit ab) = [explode ab]
	f Null = []
	f (StringLit ac) = [explode ac]
	g (BooleanLit _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((BooleanLit (TermRep.fDyn aa))::Literal) ; _ -> error "Term explosion error."
	g (IntegerLit _) xs = case TermRep.fArgs xs of [ab] -> toDyn ((IntegerLit (TermRep.fDyn ab))::Literal) ; _ -> error "Term explosion error."
	g Null xs = case TermRep.fArgs xs of [] -> toDyn ((Null)::Literal) ; _ -> error "Term explosion error."
	g (StringLit _) xs = case TermRep.fArgs xs of [ac] -> toDyn ((StringLit (TermRep.fDyn ac))::Literal) ; _ -> error "Term explosion error."

_tc_LiteralTc = mkTyCon "Literal"
instance Typeable Literal where
    typeOf x = mkTyConApp _tc_LiteralTc [ ]

instance Term BooleanLiteral where
    explode (x::BooleanLiteral) = TermRep (toDyn x, f x, g x) where
	f TRUE = []
	f FALSE = []
	g TRUE xs = case TermRep.fArgs xs of [] -> toDyn ((TRUE)::BooleanLiteral) ; _ -> error "Term explosion error."
	g FALSE xs = case TermRep.fArgs xs of [] -> toDyn ((FALSE)::BooleanLiteral) ; _ -> error "Term explosion error."

_tc_BooleanLiteralTc = mkTyCon "BooleanLiteral"
instance Typeable BooleanLiteral where
    typeOf x = mkTyConApp _tc_BooleanLiteralTc [ ]

instance Term BlockStatements where
    explode (x::BlockStatements) = TermRep (toDyn x, f x, g x) where
	f (BlockStatements aa ab) = [explode aa,explode ab]
	g (BlockStatements _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((BlockStatements (TermRep.fDyn aa) (TermRep.fDyn ab))::BlockStatements) ; _ -> error "Term explosion error."

_tc_BlockStatementsTc = mkTyCon "BlockStatements"
instance Typeable BlockStatements where
    typeOf x = mkTyConApp _tc_BlockStatementsTc [ ]

instance Term Statement where
    explode (x::Statement) = TermRep (toDyn x, f x, g x) where
	f Skip = []
	f (Block aa) = [explode aa]
	f (AssignmentStat ab) = [explode ab]
	f (InstanceCreationStat ac) = [explode ac]
	f (MethodInvocationStat ad) = [explode ad]
	f (ReturnStat ae) = [explode ae]
	f (IfStat af ag ah) = [explode af,explode ag,explode ah]
	f (WhileStat ai aj) = [explode ai,explode aj]
	f (StatFocus ak) = [explode ak]
	g Skip xs = case TermRep.fArgs xs of [] -> toDyn ((Skip)::Statement) ; _ -> error "Term explosion error."
	g (Block _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((Block (TermRep.fDyn aa))::Statement) ; _ -> error "Term explosion error."
	g (AssignmentStat _) xs = case TermRep.fArgs xs of [ab] -> toDyn ((AssignmentStat (TermRep.fDyn ab))::Statement) ; _ -> error "Term explosion error."
	g (InstanceCreationStat _) xs = case TermRep.fArgs xs of [ac] -> toDyn ((InstanceCreationStat (TermRep.fDyn ac))::Statement) ; _ -> error "Term explosion error."
	g (MethodInvocationStat _) xs = case TermRep.fArgs xs of [ad] -> toDyn ((MethodInvocationStat (TermRep.fDyn ad))::Statement) ; _ -> error "Term explosion error."
	g (ReturnStat _) xs = case TermRep.fArgs xs of [ae] -> toDyn ((ReturnStat (TermRep.fDyn ae))::Statement) ; _ -> error "Term explosion error."
	g (IfStat _ _ _) xs = case TermRep.fArgs xs of [af,ag,ah] -> toDyn ((IfStat (TermRep.fDyn af) (TermRep.fDyn ag) (TermRep.fDyn ah))::Statement) ; _ -> error "Term explosion error."
	g (WhileStat _ _) xs = case TermRep.fArgs xs of [ai,aj] -> toDyn ((WhileStat (TermRep.fDyn ai) (TermRep.fDyn aj))::Statement) ; _ -> error "Term explosion error."
	g (StatFocus _) xs = case TermRep.fArgs xs of [ak] -> toDyn ((StatFocus (TermRep.fDyn ak))::Statement) ; _ -> error "Term explosion error."

_tc_StatementTc = mkTyCon "Statement"
instance Typeable Statement where
    typeOf x = mkTyConApp _tc_StatementTc [ ]

instance Term ClassDeclaration where
    explode (x::ClassDeclaration) = TermRep (toDyn x, f x, g x) where
	f (ClassDecl aa ab ac ad ae af) = [explode aa,explode ab,explode ac,explode ad,explode ae,explode af]
	g (ClassDecl _ _ _ _ _ _) xs = case TermRep.fArgs xs of [aa,ab,ac,ad,ae,af] -> toDyn ((ClassDecl (TermRep.fDyn aa) (TermRep.fDyn ab) (TermRep.fDyn ac) (TermRep.fDyn ad) (TermRep.fDyn ae) (TermRep.fDyn af))::ClassDeclaration) ; _ -> error "Term explosion error."

_tc_ClassDeclarationTc = mkTyCon "ClassDeclaration"
instance Typeable ClassDeclaration where
    typeOf x = mkTyConApp _tc_ClassDeclarationTc [ ]

instance Term FieldDeclaration where
    explode (x::FieldDeclaration) = TermRep (toDyn x, f x, g x) where
	f (FieldDecl aa ab) = [explode aa,explode ab]
	g (FieldDecl _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((FieldDecl (TermRep.fDyn aa) (TermRep.fDyn ab))::FieldDeclaration) ; _ -> error "Term explosion error."

_tc_FieldDeclarationTc = mkTyCon "FieldDeclaration"
instance Typeable FieldDeclaration where
    typeOf x = mkTyConApp _tc_FieldDeclarationTc [ ]

instance Term ConstructorDeclaration where
    explode (x::ConstructorDeclaration) = TermRep (toDyn x, f x, g x) where
	f (ConstructorDecl aa ab ac ad) = [explode aa,explode ab,explode ac,explode ad]
	g (ConstructorDecl _ _ _ _) xs = case TermRep.fArgs xs of [aa,ab,ac,ad] -> toDyn ((ConstructorDecl (TermRep.fDyn aa) (TermRep.fDyn ab) (TermRep.fDyn ac) (TermRep.fDyn ad))::ConstructorDeclaration) ; _ -> error "Term explosion error."

_tc_ConstructorDeclarationTc = mkTyCon "ConstructorDeclaration"
instance Typeable ConstructorDeclaration where
    typeOf x = mkTyConApp _tc_ConstructorDeclarationTc [ ]

instance Term MethodDeclaration where
    explode (x::MethodDeclaration) = TermRep (toDyn x, f x, g x) where
	f (MethodDecl aa ab ac ad) = [explode aa,explode ab,explode ac,explode ad]
	g (MethodDecl _ _ _ _) xs = case TermRep.fArgs xs of [aa,ab,ac,ad] -> toDyn ((MethodDecl (TermRep.fDyn aa) (TermRep.fDyn ab) (TermRep.fDyn ac) (TermRep.fDyn ad))::MethodDeclaration) ; _ -> error "Term explosion error."

_tc_MethodDeclarationTc = mkTyCon "MethodDeclaration"
instance Typeable MethodDeclaration where
    typeOf x = mkTyConApp _tc_MethodDeclarationTc [ ]

instance Term FormalParameters where
    explode (x::FormalParameters) = TermRep (toDyn x, f x, g x) where
	f (FormalParams aa) = [explode aa]
	g (FormalParams _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((FormalParams (TermRep.fDyn aa))::FormalParameters) ; _ -> error "Term explosion error."

_tc_FormalParametersTc = mkTyCon "FormalParameters"
instance Typeable FormalParameters where
    typeOf x = mkTyConApp _tc_FormalParametersTc [ ]

instance Term FormalParameter where
    explode (x::FormalParameter) = TermRep (toDyn x, f x, g x) where
	f (FormalParam aa ab) = [explode aa,explode ab]
	g (FormalParam _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((FormalParam (TermRep.fDyn aa) (TermRep.fDyn ab))::FormalParameter) ; _ -> error "Term explosion error."

_tc_FormalParameterTc = mkTyCon "FormalParameter"
instance Typeable FormalParameter where
    typeOf x = mkTyConApp _tc_FormalParameterTc [ ]

instance Term VariableDeclaration where
    explode (x::VariableDeclaration) = TermRep (toDyn x, f x, g x) where
	f (VariableDecl aa ab) = [explode aa,explode ab]
	g (VariableDecl _ _) xs = case TermRep.fArgs xs of [aa,ab] -> toDyn ((VariableDecl (TermRep.fDyn aa) (TermRep.fDyn ab))::VariableDeclaration) ; _ -> error "Term explosion error."

_tc_VariableDeclarationTc = mkTyCon "VariableDeclaration"
instance Typeable VariableDeclaration where
    typeOf x = mkTyConApp _tc_VariableDeclarationTc [ ]

instance Term Type where
    explode (x::Type) = TermRep (toDyn x, f x, g x) where
	f INT = []
	f BOOLEAN = []
	f (Type aa) = [explode aa]
	g INT xs = case TermRep.fArgs xs of [] -> toDyn ((INT)::Type) ; _ -> error "Term explosion error."
	g BOOLEAN xs = case TermRep.fArgs xs of [] -> toDyn ((BOOLEAN)::Type) ; _ -> error "Term explosion error."
	g (Type _) xs = case TermRep.fArgs xs of [aa] -> toDyn ((Type (TermRep.fDyn aa))::Type) ; _ -> error "Term explosion error."

_tc_TypeTc = mkTyCon "Type"
instance Typeable Type where
    typeOf x = mkTyConApp _tc_TypeTc [ ]

--  Imported from other files :-