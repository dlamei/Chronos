# Chronos
<sup> version 0.6 <sup>

---

## Idea

very few and simple building blocks, that allows for complex systems to emerge, inspired by lisp
no functions, no classes, only expressions, and primitives

## Primitives

u8, i8, i16, u16, i32, u32, i64, u64, i128, u128, f32, f64, string, char, bool
every primitve acts like an expression

## Expressions

expressions are made up of two things:

Hashmap:\
The keys are the variable names, and the value is the value of the variable
the hashmap of the expression can be updated outside of the expression

expression body:\
the body can be evaluated and can update the hashmap
the body is just a list of other expressions

## Builtin expressions:

```rust
if(expr, expr, expr = {})
loop(expr, expr, expr)
print(&expr);
length(&expr); //returns the length of an expression body
get_expr(&expr, indx); //returns an adress to the i-th expression in an expression
insert_expr(&expr, indx); //insert an expression at indx (-1 for insert at end)
combine_expr(&exprA, &exprB); //creates new expression from exprA and exprB
remove_expr(&expr, indx); //removes the i-th expression from the expression body
Ref(type); //(& keyword expands to Ref(...)) the type is passed just like any argument
Null; //nullpointer
```

chronos is dynamically typed, but there is type anotation:

```rust
i: i32 = 3;
i = "Hello"; //error
s: auto = "Hello"; // auto deduces type and acts the same as s: string in this case
const val = 3; //value can't be changed
```

### uninitialized variables

```rust
val: i32; //this tells chronos that the val integer exists.
//accessing this variable in any way before initializing it results in an error
```

### more examples:

```rust
e = { i: i32 = 3; print("Hello World") }; //e is an expression
//nothing yet printed
e.i; //undefined
e(); //evaluate expression, prints: Hello World
e.i; // = 3

e = { print(val) }; //possible, even if val is not defined

e(); //error: val not defined
val: f32 = 4.3;
e(); // prints: 4.3

(e = { value: i32 })(); //this tells chronos that the integer "value" is defined;
e.value; //uninitialized error
e.value = 3;
e.value; // 3
//you can simulate static variables
count = 0;
e = { print(count++); };
e() //0
e() //1

e = { i: i32 = 3; }(); //evaluate it
e.f = 32.3;
e.f; // 32.3
```

### copying expressions:

when you assign one variable to another it creates a deep copy of the expression body and the expression hashmap
this means it will deep-copy every expression in the body

```rust
f = e; //creates a deep copy of the hashmap and the function body
```

### expression signature:

expect some values when calling expression, and set the type that is returned:

```rust
e = (val1: f32, val2: f32 = 0) -> f32 { print(val1); val1 + val2 }; //same as in rust return keyword is not needed
e() //val not set
e(4.3)
e(val1 = 4.3, 3) //set by name
```

### type anotation for expression:

```rust
add: (f32, f32) -> f32;
//unknown return type:
add: (f32, f32);
//no return type
add: (f32, f32) -> void
//any expression as type:
add: (...);
add: (...) -> f32;
//expressions can be returned
exp: (...) -> (...);
exp: (...) -> (f32, f32) -> f32; //returns expression with type (f32, f32) -> f32
```

### if / else

```rust
//if first expression is true eval second expression
if({...},
{
//evaluated if true
}, {
//evaluated if false
//this expression is optional
});
```

### loop

just an expression with 3 expressions as param:\
first expression gets called at the beginning, second loop is continued as long as it returns true, third is evaluated every loop\
**TODO**: iteratos
```rust
loop({i = 0}, {i++ < u32}, {
	print(i);
});
//maybe expressions be be deduced, no {} necessary:
loop(i = 0, i++ < u32,
	print(i);
);
```

### class like expressions:

expresssions can be used as types for other expressions
the body of the expression acts like some sort of constructor

```rust
const Vec = {
x: i32 = 0;
y: i32 = 0;
z: i32 = 0;

    add: (i32, i32, i32) -> &Vector =
    	(x: i32, y: i32, z: i32) -> &Vector {
    		this.x += x; //"this" points to the parent Expression of this expression (so Vec in this case)
    		this.y += y;
    		this.z += z;

    		this //return this
    	};

    //__add__ = add; (maybe some operator overloader)
    //__eval__ = delete //maybe some way to delete eval function

}(); //evaluate so that the fields are initialized

(v1 = Vec)(); //first create a copy and then call the eval on v1 (in this case not really necessary)
//with this method the vector v1 and the type Vec are identical
```

### **TODO** define:

with this the expression gets bound to the variable Vec
every other variable that copies from this contain the information that it was copied from Vec
creates a difference between type and variable

```rust
// ⌄⌄ define
Vec := {
...
};
```

### Interfaces:

expressions can be "donwcasted".
hereby the variables with the same name get assigned to eachother
this can be used to create an interface for other expresssions, 
so you later know exactly what expressions are defined.

```rust
(v1 = Vec)();
coord: {x; y;} = v1; //copies v1.x to coord.x and v1.y to coord.y

const Addable = {
add: (i32, i32) -> &Addable //this syntax tells chronos that the add expression is defined, just not here;
} // these expression can be used to specify what expressions are defined in another expression

addable: Addable = v1; //v1 gets downcasted (if the add expression did not exist in v1 it would throw an error)
addable.x; //undefined
addable.add(3, 4); //defined, returns another addable

addable_ref: Ref(Addable) = &v1; //works with references

foo = (&Addable: a, i32: x, i32: y) -> &Addable {
a.add(x, y)
};
foo(&v1); // &v1 gets auto downcasted
```

### Array implementations:

**linked lists**

```rust
const arr = (type) {
val: type,
next: &arr,
get = (n: u32) -> type;
}();

arr.get = (n: u32) -> arr.type {
current = this;

    for ({i = 0}, {i++ < u32}) {
    		current = current.next; //maybe some way of error checking
    };

    current.val

};
```

**c-like array**

```rust
const int_arr = {
get = (indx: u32) -> Ref(i32);
push_back = (val: i32) -> void;
pop = (indx: u32) -> i32;

    i32(1); i32(2); i32(3); //works because numbers are also expressions

}()

print(length(&int_arr)); // 6
print(get_expr(&int_arr, 4)); // in32(2)

int_arr.get = (indx: u32) -> Ref(i32) {
	get_exr(this, indx + 3) //we don't want to return the get, push, and pop expressions
};

int_arr.push_back = (val: i32) -> void {
	this = combine_expr(this, val); //one more number gets added to the expression body
};

int_arr.pop = (indx: u32) -> i32 {
v: i32 = this.get(indx);
	remove_expr(this, indx);
};
```
