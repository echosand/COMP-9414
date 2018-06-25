tree_eval(Value, tree(empty,X,empty), Eval) :- X=z, Eval is Value. 
tree_eval(Value, tree(empty,X,empty), Eval) :- number(X), Eval is X.

tree_eval(Value, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Value, Left, LeftEval), 
　　 tree_eval(Value, Right, RightEval),
     X = '+',
     Eval is LeftEval + RightEval.
tree_eval(Value, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Value, Left, LeftEval), 
　　 tree_eval(Value, Right, RightEval),
     X = '-',
     Eval is LeftEval - RightEval.
tree_eval(Value, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Value, Left, LeftEval), 
　　 tree_eval(Value, Right, RightEval),
     X = '*',
     Eval is LeftEval * RightEval.
tree_eval(Value, tree(Left,X,Right), Eval) :- 
　　 tree_eval(Value, Left, LeftEval), 
　　 tree_eval(Value, Right, RightEval),
     X = '/',
     Eval is LeftEval / RightEval.
