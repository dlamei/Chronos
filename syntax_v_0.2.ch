//function_desc:
//() -> () undef. expression that returns undef. expression
//() without the arrow the return type is also undef.
//language is dynamically typed, but there is type annotation

//TODO: how to cast, (based on variable names with downcasting), (check for specific expression id?)

{

	e: () -> () = {
		print("Hello World");
	};
	e(); // "Hello World"
			 // e() returns null?
	
	global_x: i32 = 3;
	{
		print(global_x);
	}() //  3
	
	add_int: (i32, i32) -> i32 = (x: i32, y: i32) -> i32 {
		x + y
	};
	
	add_int() // x and y not defined
	add_int(3, 2); // 5
	
	count = 0;
	
	expr: () -> () = {
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

	v1 = Vector();
	
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

}() //entire code is an expression an must be called
