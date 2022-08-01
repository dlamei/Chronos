//function_desc:
//{...} this as type could be any expression
//() -> {} no parameters and return nothing
//() -> {...} no parameters and returns undef. expression
//() without the arrow the return type is undef.
//language is dynamically typed, but there is type annotation

//TODO: how to cast, (based on variable names with downcasting), (check for specific expression id?)
//TODO: look at const
//TODO: add auto
//TODO: combine expressions (maybe also iterate throuh all expressions in an expression)
//TODO: default parameter value, set parameter by name

{

	const e: () -> {} = { // returns nothing
		print("Hello World");
	};
	e(); // "Hello World"
			// e() return null
	
	global_x: i32 = 3;
	{
		print(global_x);
	}() //  3
	
	add_int: auto = (x: i32, y: i32) -> i32 { //auto is in this case (i32, i32) -> i32
		x + y
	};
	
	add_int() // x and y not defined
	add_int(3, 2); // 5
	
	count = 0;
	
	expr: {...} = { //not same as auto because expr can still be changed to any other expression
		count: i32 = ::count++; //:: global namespace
		test: i32 = 0;
	}
	
	expr.count; //not defined
	
	expr();
	expr.count; // = 0
	expr();
	expr.count; // = 1
							
	//Expression comparison:
	test = expr; //creates a deep copy of the expression
	test == expr; // true
	
	//try to use this as some form of template
	const Vector = { // the constructor can also be add as a func_desc
		x: i32 = 0;
		y: i32 = 0;
	
		add: (i32, i32) -> &Vector; //every vector has a add function
	};

	(v1 = Vector)(); //first create a copy then eval it
	
	Vector::new() = (x: i32, y: i32) { //if defined like this the v1 Vector would not contain this function
		(v = Vector)(); //copy of Vector expression then initialized
		
		v.x = x;
		v.y = y;
	
		ref(v); //return reference to the vector, or return copy?
	}
	
	Vector::add(x: i32, y: i32) -> &Vector {
		this.x += x; //this points to the context from where this expression was defined
		this.y += y; //this is always defined
	
		return ref(this);
	}

	foo: auto = (x: i32, y: i32, expr: {...}) { // expr can be any expression and return any exression (because no arrow)
		expr() //eval expr
	}

	foo(2, 3 {x + y}) //returns 5, because x and y get defined in foo

}() //entire code is an expression an must be called
