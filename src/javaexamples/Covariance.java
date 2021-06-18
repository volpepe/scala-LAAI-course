package javaexamples;

class Base {
    public void f(){
        System.out.println("Base class");
    }
}

class Derived extends Base {
    public void f() {
        System.out.println("Derived class");
    }
    public void g() {
        // Only class Derived has method g()
        System.out.println("Only in derived class");
    }
}

public class Covariance {
    public static void main(String[] args) {
        // Consider Derived to be a subtype of type Base.
        // (Derived extends Base)
        Derived[] arr1 = new Derived[10];
        // We can then assign an Array of type Derived to an Array of type Base,
        // because the arrays are in a covariant relationship.
        Base[] arr2 = arr1;
        // We can of course insert a Base object on the first cell of the array.
        // The compilation of this class will not generate any problem. Idea sees the
        // problem because it is more intelligent ;)
        arr2[0] = new Base();
        // Note that arr1 and arr2 are alias for the same array with the assignment above.
        // since arr1 is of type Derived, we can call the function g on one of its elements.
        // Note: we can't invoke g() on arr2, because the compiler sees the static type of arr2
        // which is Base[] and generates an error at compilation time. This is caused by the dynamic typing.
        arr1[0].g(); // but actually we have stored a Base element on the first element!
        // This will cause an ArrayStoreException, but only at runtime!!.
        // This is caused by covariance.

        // Covariance is useful and safe only when data structures are immutable.
        // For example, this is why Lists (immutable type) are safer than arrays.

        // Scala keeps Arrays mutable and does not admit them to be covariant,
        // therefore throwing an error at compile time.
    }
}
