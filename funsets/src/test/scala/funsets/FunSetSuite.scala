package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect only contains the elements on both sets") {
    new TestSets {
      val s = intersect(s1, s2)
      val su = union(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(contains(su, 2), "Intersect 2")
      assert(contains(su, 2), "Intersect 2")
    }
  }

  test("diff contains the elements in s2 but not s3") {
    new TestSets {
      val s = diff(s2, s3)
      assert(contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
    }
  }

  test("filter from the elements that are accepted by predicate") {
    new TestSets {
      assert(!contains(filter(s1, x => x > 1), 1), "Filter 1")
      assert(contains(filter(s2, x => x > 1), 2), "Filter 2")
      assert(contains(filter(s3, x => x > 1), 3), "Filter 3")
    }
  }

  test("for all positive numbers") {
    new TestSets {
      val s = union(s1, union(s2, s3))
      assert(forall(s, x => x > 0), "forall positive")
      assert(!forall(s, x => x > 1), "forall greater than 1")
    }
  }
  test("forall upper limit") {
    new TestSets {
      assert(forall(singletonSet(bound), x => x > 1), "forall uppper limit")
    }
  }

  test("forall for empty set") {
    new TestSets {
      assert(forall(intersect(s1,s2), x => x > 1), "forall empty set")
    }
  }

  test("exists at least one element greater than 1") {
    new TestSets {
      assert(exists(union(s1, s2), x => x > 1), "exists element greater than 1")
      assert(!exists(union(s1, s2), x => x > 2), "exists element greater than 2")
    }
  }

  test("exists for empty set") {
    new TestSets {
      assert(!exists(intersect(s1,s2), x => x > 1), "exists empty set")
    }
  }

  test("map multiplying by 2") {
    new TestSets {
      assert(contains(map(s1, x => x * 2), 2), "map multiplying by 2 set s1")
      assert(contains(map(s2, x => x * 2), 4), "map multiplying by 2 set s2")
      assert(contains(map(s3, x => x * 2), 6), "map multiplying by 2 set s3")
    }
  }
}
