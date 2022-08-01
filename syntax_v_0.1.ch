//use symbols for ref, deref, eval

//how to determine return type

//used names:
// eval, print, ref, deref, fn, ret, Expr, String, Any

//use () as eval? 
// eval an expression with () and bind some variables
//f = (x: i32, expr: Expr) -> i32 {
//	print(x);
//	4
//}
//f() // f undefined
//f(y = 3); // deduce expression
//eval
//f(3);
//or use function lambda as an expression
//lambda: add : (i32, f32) -> i32

eval({

	count = 0;

	expr: Expr = {
		count: i32 = ::count++;
		test: i32 = 0;
	}

	expr.count; // not defined

	expr.eval();
	expr.count; // = 0

	expr.eval();
	expr.count; // = 1
	
	print(this); //reference to the scope it is in

	//maybe act as a template
	eval(Point = { //evaluating this could be usefull
		x: i32;
		y: i32;

		count: i32;

		add: fn(x: i32, y: i32, RET: &Point, Expr);
	});

	Point::new() = fn(x: i32 = 0, y: i32 = 0, ret = &Point, {
		this.count++; //this is a reference to Point
		p = Point; // create a copy
		eval(p);
		//or p()

		p.x = x;
		p.y = y;

		ref(p)
	});

	p : Point = Point::new();
	p.add();

	foo = fn(x: i32, expr: Expr, {
		if (defined(x))
			eval(expr);
		else ...
	})

	foo(3, { print("value = {}", x) });

	bind = fn(expr: Expr, func: Expr, {
		eval(expr);
		eval(func)
	})

	bind({x = 1; y = 1}, {func(x, y)})

})
