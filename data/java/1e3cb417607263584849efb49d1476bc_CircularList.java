// Implementation of circular lists, using singly linked elements.
// (c) 1998, 2001 duane a. bailey

package structure5;
import java.util.Iterator;
/**
 * An implementation of lists using circularly linked elements, 
 * similar to that of {@link java.util.LinkedList java.util.LinkedList}.
 * <p>       
 * This class is an implementation of the {@link List} interface.
 * Operations accessing or modifying either the head or the tail of 
 * the list execute in constant time.
 * Circular lists are as space-efficient as singly linked lists, 
 * but tail-related operations are less costly.
 * <p>
 * Example usage:
 *
 * To place a copy of every unique parameter passed to a program into a 
 * CircularList,  we would use the following:
 * <pre>
 * public static void main({@link java.lang.String String[]} arguments)
 * {
 *     {@link CircularList} argList = new {@link #CircularList()};
 *     for (int i = 0; i < arguments.length; i++){
 *         if (!argList.{@link #contains(Object) contains(arguments[i])}){
 *             argList.{@link #add(Object) add(arguments[i])};
 *         }
 *    }
 *    System.out.println(argList);
 * }
 * </pre>
 * @version $Id: CircularList.java 31 2007-08-06 17:19:56Z bailey $
 * @author, 2001 duane a. bailey
 * @see SinglyLinkedList
 * @see DoublyLinkedList
 */
public class CircularList<E> extends AbstractList<E>
{
    /**
     * A reference to tail of list.  tail points to head.
     */
    protected Node<E> tail;
    /**
     * Number of elements within circular list.
     */
    protected int count;

    /**
     * Construct an empty circular list.
     *
     * @pre constructs a new circular list
     */
    public CircularList()
    {
        tail = null;
        count = 0;
    }

    /**
     * Add an element to head of circular list.
     *
     * @post adds value to beginning of list
     * 
     * @param value value to be added to list.
     */
    public void add(E value)
    {
        addFirst(value);
    }

    /**
     * Add an element to head of list.
     *
     * @pre value non-null
     * @post adds element to head of list
     * 
     * @param value  value added to head of list.       
     */
    public void addFirst(E value)
    {
        Node<E> temp = new Node<E>(value);
        if (tail == null) { // first value added
            tail = temp;
            tail.setNext(tail);
        } else { // element exists in list
            temp.setNext(tail.next());
            tail.setNext(temp);
        }
        count++;
    }

    /**
     * Add a value to tail of circular list.
     *
     * @pre value non-null
     * @post adds element to tail of list
     * 
     * @param value value to be added.
     */
    public void addLast(E value)
    {
        // new entry:
        addFirst(value);
        tail = tail.next();
    }

    /**
     * Determine if a list is empty.
     *
     * @pre !isEmpty()
     * @post returns value at head of list
     * 
     * @return True if there are no elements within list.
     */
    public E getFirst()
    {
        return tail.next().value();
    }

    /**
     * Peek at last element of list.
     *
     * @pre !isEmpty()
     * @post returns value at tail of list
     * 
     * @return value of last element of list.
     */
    public E getLast()
    {
        return tail.value();
    }

    /**
     * Remove a value from head of list.
     *
     * @pre !isEmpty()
     * @post returns and removes value from head of list
     * 
     * @return value removed.
     */
    public E removeFirst()
    {
        Node<E> temp = tail.next(); // ie. head of list
        if (tail == tail.next()) {
            tail = null;
        } else {
            tail.setNext(temp.next());
            temp.setNext(null); // helps clean things up; temp is free
        }
        count--;
        return temp.value();
    }

    /**
     * Remove a value from tail of list.        
     * 
     * @pre !isEmpty()
     * @post returns and removes value from tail of list
     * 
     * @return  value removed.
     */
    public E removeLast()
    {
        Assert.pre(!isEmpty(),"list is not empty.");
        Node<E> finger = tail;
        while (finger.next() != tail) {
            finger = finger.next();
        }
        // finger now points to second-to-last value
        Node<E> temp = tail;
        if (finger == tail)
        {
            tail = null;
        } else {
            finger.setNext(tail.next());
            tail = finger;
        }
        count--;
        return temp.value();
    }

    /**
     * Check if a list contains an element.
     *
     * @pre value != null
     * @post returns true if list contains value, else false
     * 
     * @param value  value sought.
     * @return True iff value is within list.
     */
    public boolean contains(E value)
    {
        if (tail == null) return false;

        Node<E> finger;
        finger = tail.next();
        while ((finger != tail) && (!finger.value().equals(value)))
        {
            finger = finger.next();
        }
        return finger.value().equals(value);
    }

    /**
     * Remove a value from  a list.
     *
     * @pre value != null
     * @post removes and returns element equal to value, or null
     * 
     * @param value value sought.
     * @return value removed from list.
     */
    public E remove(E value)
    {
        if (tail == null) return null;
        Node<E> finger = tail.next();
        Node<E> previous = tail;
        int compares;
        for (compares = 0;
             (compares < count) && (!finger.value().equals(value));
             compares++) 
        {
            previous = finger;
            finger = finger.next();
        }
        if (finger.value().equals(value)) {
            // an example of pigeon-hole principle
            if (tail == tail.next()) { tail = null; }
            else {
                if (finger == tail) tail = previous;
                previous.setNext(previous.next().next());
            }
            // finger value free
            finger.setNext(null); // to keep things disconnected
            count--;            // fewer elements
            return finger.value();
        } else return null;
    }

    /**
     * Determine size of list.
     *
     * @post returns number of elements in list
     * 
     * @return  number of elements in list.
     */
    public int size()
    {
        return count;
    }

    /**
     * Get value at location i.
     *
     * @pre 0 <= i < size()
     * @post returns object found at that location
     *
     * @param i position of value to be retrieved.
     * @return value retrieved from location i (returns null if i invalid)
     */
    public E get(int i)
    {
        if (i >= size()) {
            return null;
        }
        Node<E> finger = tail.next();
        // search for ith element or end of list
        while (i > 0)
        {
            finger = finger.next();
            i--;
        }
        // return value found
        return finger.value();
    }

    /**
     * Accessor method for tail field
     */
    protected Node<E> getTail(){
        return tail;
    }

    /**
     * Set value stored at location i to object o, returning old value.
     *
     * @pre 0 <= i < size()
     * @post sets ith entry of list to value o, returns old value
     * @param i location of entry to be changed.
     * @param o new value
     * @return former value of ith entry of list.
     */
    public E set(int i, E o)
    {
        if (i >= size()) return null;
        Node<E> finger = tail.next();
        // search for ith element or end of list
        while (i > 0)
        {
            finger = finger.next();
            i--;
        }
        // get old value, update new value
        E result = finger.value();
        finger.setValue(o);
        return result;
    }

    /**
     * Insert value at location.
     *
     * @pre 0 <= i <= size()
     * @post adds ith entry of list to value o
     * @param i index of this new value
     * @param o value to be stored
     */
    public void add(int i, E o)
    {
        Assert.pre((0 <= i) && (i <= size()),"Index in range.");
        if (i == 0) addFirst(o);
        else if (i == size()) addLast(o);
        else {
            Node<E> previous = tail;
            Node<E> next = tail.next();
            while (i > 0)
            {
                previous = next;
                next = next.next();
                i--;
            }
            Node<E> current = new Node<E>(o,next);
            count++;
            previous.setNext(current);
        }
    }

    /**
     * Remove and return value at location i.
     *
     * @pre 0 <= i < size()
     * @post removes and returns object found at that location
     *
     * @param i position of value to be retrieved.
     * @return value retrieved from location i (returns null if i invalid)
     */
    public E remove(int i)
    {
        Assert.pre((0 <= i) && (i < size()),"Index in range.");
        // if not in list, return nothing
        if (i == 0) return removeFirst();
        if (i == size()-1) return removeLast();
        Node<E> previous = tail;
        Node<E> finger = tail.next(); // ie. head
        // count to appropriate location
        while (i > 0)
        {
            i--;
            previous = finger;
            finger = finger.next();
        }
        // unlink finger'd object
        previous.setNext(finger.next());
        count--;
        return finger.value();
    }

    /**
     * Determine first location of a value in list.
     *
     * @pre value is not null
     * @post returns (0-origin) index of value,
     *   or -1 if value is not found
     * 
     * @param value value sought.
     * @return index (0 is first element) of value, or -1
     */
    public int indexOf(E value)
    {
        int i = 0;
        Node<E> finger = tail.next();
        // search for value or end of list, counting along way
        while (finger != null && !finger.value().equals(value))
        {
            if (finger == tail) { // we fall off list
                finger = null;
            } else {
                finger = finger.next();
            }
            i++;
        }
        // finger points to value, i is index
        if (finger == null)
        {   // value not found, return indicator
            return -1;
        } else {
            // value found, return index
            return i;
        }
    }

    /**
     * Determine last location of a value in list.
     *
     * @pre value is not null
     * @post returns (0-origin) index of value,
     *   or -1 if value is not found
     * 
     * @param value value sought.
     * @return index (0 is first element) of value, or -1
     */
    public int lastIndexOf(E value)
    {
        int result = -1;        // assume not found, return -1
        int i = 0;
        Node<E> finger = tail.next();
        // search for last matching value, result is desired index
        while (finger != null)
        {
            // a match? keep track of location
            if (finger.value().equals(value)) result = i;
            if (finger == tail) {
                finger = null;
            } else {
                finger = finger.next();
            }
            i++;
        }
        // return last match
        return result;
    }

    /**
     * Construct an iterator over elements of list.
     * Elements are traversed from head to tail.
     *
     * @post returns iterator to traverse list elements
     * 
     * @return iterator associated with list.
     */
    public Iterator<E> iterator()
    {
        return new CircularListIterator<E>(tail);
    }

    /**
     * Determine if a list is empty.
     *
     * @post returns true if no elements in list
     * 
     * @return True iff list is empty.
     */
    public boolean isEmpty()
    {
        return tail == null;
    }

    /**
     * Remove elements of list.
     *
     * @post removes all elements from list
     */
    public void clear()
    {
        count = 0;
        tail = null;
    }

    /**
     * Generate a string representation of list.
     *
     * @post returns a string representing list
     * 
     * @return String representing list.
     */
    public String toString()
    {
        StringBuffer s = new StringBuffer();
        s.append("<CircularList:");
        Iterator li = iterator();
        while (li.hasNext())
        {
            s.append(" "+li.next());
        }
        s.append(">");
        return s.toString();
    }
}


