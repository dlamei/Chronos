
//goal: very few and simple building blocks, that allows for complex systems to emerge, inspired by lisp
//no functions, no classes, only expressions, and primitives

//primitives (can't be null):
// u8, i8, i16, u16, i32, u32, i64, u64, i128, u128, string, char = u8
// builtin "classes":
// Ref(type) can be null (& keyword is used)
 
//chronos is dynamically typed, but there is type anotation:
i: i32 = 3;
i = "Hello" //error
s: auto = "Hello" // auto deducec type and acts the same as s: string in this case
const val = 3; //value can't be changed

e = { i: i32 = 3; print("Hello World") }; //e is an expression
																				 //nothing yet printed
e.i;	//undefined
e(); //evaluate expression, prints: Hello World
e.i; // = 3

e = { print(val) }; //possible, even if i is not defined

e(); //error: i not defined
val: f32 = 4.3;
e(); // prints: 4.3
		 
//you can simulate static variables
count = 0;
e = { print(count++); };
e() //0
e() //1

//expressions are made up of two things:
//Hashmap: the keys are the variable names, and the value is the value of the variable
//expression body: the body can be evaluated and can update the hashmap
//the hashmap of the expression can be updated outside of the expression:
//TODO: maybe some way to edit expression body at runtime (meta programming at runtime)

e = { i: i32 = 3; }(); //evaluate it
e.f = 32.3;
e.f; // 32.3

//copying expressions:
f = e;
//creates a deep copy of the hashmap and the function body

//expression signature:
//expect some values when calling expression, and set the type that is returned:
e = (val1: f32, val2: f32 = 0) -> f32 { print(val1); val1 + val2 }; //same as in rust return keyword is not needed
e() //val not set
e(4.3)
e(val1 = 4.3, 3) //set by name

//type anotation of an add expression
add: (f32, f32) -> f32;
//unknown return type:
add: (f32, f32);
//no return type
add: (f32, f32) -> void
//any expression as type:
add: (...);
add: (...) -> f32;
//expressions can be returned
//should probably not be done often
exp: (...) -> (...);
exp: (...) -> (f32, f32) -> f32; //returns expression with type (f32, f32) -> f32

//classes:

const Vec = {
	x: i32 = 0;
	y: i32 = 0;

	add: (i32, i32) -> &Vector = 
		(x: i32, y: i32) -> &Vector {
			this.x += x; //"this" points to the parent Expression of this expression (so Vec in this case)
			this.y += y;

			this //return this
		}

	//__add__ = add; (maybe some operator overloader)
	//__eval__ = delete //maybe some way to delete eval function
};  //in this case the body of the expression is like a constructor, no public or private exists (yet).


(v1 = Vec)() //first create a copy and then call the eval on v1
						 //TODO: seems a bit anoying...
//with this method the vector v1 and the type Vec are identical
//maybe use some sort of define:

//TODO:
//	⌄⌄ define
Vec := {
	...
} //with this the expression gets bound to the variable Vec
	//every other variable that copies from this contain the information that it was copied from Vec
	//creates a difference between type and variable

//TODO: unsure if usefull
//TODO: maybe something more simmilar to traits
//casting: expressions can be donwcasted:
//hereby the variables that exist in the downcast class get assigned the value of the super class
const Addable = {
	add: (i32, i32) -> &Addable
}

addable: Addable = v1; //v1 gets downcasted
addable.x; //undefined
addable.add(3, 4) //defined, returns another addable

foo = (Addable: a, i32: x, i32: y) -> &Addable {
	a.add(x, y)
}
foo(v1) // v1 gets auto downcasted

//TODO: unsure concepts:

//for loop, just an expression with 3 expressions as param:
//first expression gets called at the beginning, second loop is continued as long as it returns true, third is evaluated every loop
//TODO: maybe some iterator
//TODO: for loop not possible this way?
for({i = 0}, {i++ < u32}, {
			print(i);
		}) ;
//maybe expressions be be deduced, no {} necessary:
for(i = 0, i++ < u32, 
			print(i);
		);

//TODO: combine expressions (maybe also iterate throuh all expressions in an expression)
//TODO: what about arrays?
//maybe with self configuring expressions? e.g:
const arr = {
	i32, i32, i32 //with push the expression body gets configured to add one more i32? 
} // is an array of 3 i32's a[0] returns first expression in body
	// seems too complicated

//linked lists? works better
const arr = (type) {
	val: type,
	next: &arr,
	
	get = (n: u32) -> type;
}

arr.get = (n: u32) -> arr.type {
	current = this;

	for ({i = 0}, {i++ < u32}) {
			current = current.next; //maybe some way of error checking
	}

	current.val
}

