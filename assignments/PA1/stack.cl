(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class List {
   -- Define operations on empty lists.

   isNil() : Bool { true };

   -- Since abort() has return type Object and head() has return type
   -- String, we need to have an String as the result of the method body,
   -- even though abort() never returns.

   head()  : String { { abort(); ""; } };

   -- As for head(), the self is just to make sure the return type of
   -- tail() is correct.

   tail()  : List { { abort(); self; } };

   -- When we cons and element onto the empty list we get a non-empty
   -- list. The (new Cons) expression creates a new list cell of class
   -- Cons, which is initialized by a dispatch to init().
   -- The result of init() is an element of class Cons, but it
   -- conforms to the return type List, because Cons is a subclass of
   -- List.

   cons(i : String) : List {
      (new Cons).init(i, self)
   };

};


(*
 *  Cons inherits all operations from List. We can reuse only the cons
 *  method though, because adding an element to the front of an emtpy
 *  list is the same as adding it to the front of a non empty
 *  list. All other methods have to be redefined, since the behaviour
 *  for them is different from the empty list.
 *
 *  Cons needs two attributes to hold the string of this list
 *  cell and to hold the rest of the list.
 *
 *  The init() method is used by the cons() method to initialize the
 *  cell.
 *)

class Cons inherits List {

   car : String;	-- The element in this list cell

   cdr : List;	-- The rest of the list

   isNil() : Bool { false };

   head()  : String { car };

   tail()  : List { cdr };

   init(i : String, rest : List) : List {
      {
	 car <- i;
	 cdr <- rest;
	 self;
      }
   };

};

class StackMachine {

    stack:List <- new List;

    get_input(c: String): Object {
        if c = "e" then
            if not stack.isNil() then 
                (let h: String <- stack.head() in
                if h = "+" then
                    add()
                else if h = "s" then
                    swap()
                else
                    new Object
                fi fi
                )
            else
                new Object
            fi
        else if c = "d" then 
            display()
        -- else if c = "x" then
        -- nothing
        else
        {
            stack <- stack.cons(c);
        }
        fi fi
    };

    pop(): String {
        (let popped_element: String <- stack.head() in {
            stack <- stack.tail();
            popped_element;
        }
        )
    };

    add(): Object {
        {
            -- pop "+"
            pop();
            -- add
            (let a_1: String <- pop(), a_2: String <- pop(), conv: A2I <- (new A2I) in 
                stack <- stack.cons(conv.i2a(conv.a2i(a_1) + conv.a2i(a_2)))
            );
        }
    };

    swap(): Object {
        {
            -- pop "s"
            pop();
            -- swap
            (let s_1: String <- pop(), s_2: String <- pop() in
                {
                    stack <- stack.cons(s_1);
                    stack <- stack.cons(s_2);
                }
            );
        }
    };
    
    display(): Object {
        (let l: List <- stack in
            while(not l.isNil()) loop {
                (new IO).out_string(l.head().concat("\n"));
                l <- l.tail();
            }
            pool
        )
    };

};

class Main inherits IO {

   main() : Object {
        (let str: String, sm: StackMachine <- (new StackMachine) in 
            while (not str = "x") loop {
            out_string(">");
            str <- in_string();
            out_string(str.concat("\n"));
            sm.get_input(str);
            }
            pool
        )
   };

};