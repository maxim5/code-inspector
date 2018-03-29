// Implementation of queues using linked lists.
// (c) 1998, 2001 duane a. bailey

package structure5;
import java.util.Iterator;

/**
 * An implementation of queues based on circular lists.
 * The head of the queue is stored at the head of the list, allowing the queue to 
 * grow and shrink in constant time. 
 * This queue implementation is ideal for applications that require a dynamically
 * resizable queue that resizes in constant time.
 * <P>
 * Example usage:
 * <P>
 * To compute the sum of the unicode value of every character in the standard input
 * we could use the following:
 * <P>
 * <pre>
 * public static void main(String[] arguments)
 * {
 *     {@link QueueList} q = new {@link #QueueList()};
 *     int unicodeSum = 0;
 *
 *     if(arguments.length > 0){
 *         for(int i=0; i < arguments.length; i++){
 *             for(int j=0; j < arguments[i].length(); j++){
 *                 q.{@link #enqueue(Object) enqueue(new Character(arguments[i].charAt(j)))};
 *             }
 *         }
 *     }
 *
 *     while(!q.{@link #empty()}){
 *        char c = ((Character)q.{@link #dequeue()}).charValue();
 *        unicodeSum+=Character.getNumericValue(c);
 *     }
 *
 *     System.out.println("Total Value: " + unicodeSum);
 * }
 * </pre>
 * @see QueueArray
 * @see QueueVector
 * @version $Id: QueueList.java 22 2006-08-21 19:27:26Z bailey $
 * @author, 2001 duane a. bailey
 */
public class QueueList<E> extends AbstractQueue<E> implements Queue<E>
{
    /**
     * The list holding queue values in order.
     */
    protected List<E> data;

    /**
     * Construct a new queue with no data.
     *
     * @post Constructs a new, empty queue
     */
    public QueueList()
    {
        data = new CircularList<E>();
    }

    /**
     * Add a value to the tail of the queue.
     *
     * @post The value is added to the tail of the structure
     * 
     * @param value The value added.
     * @see #enqueue
     */
    public void add(E value)
    {
        data.addLast(value);
    }

    /**
     * Remove a value from the head of the queue.
     *
     * @pre The queue is not empty
     * @post The head of the queue is removed and returned
     * 
     * @return The value actually removed.
     * @see #dequeue
     */
    public E remove()
    {
        return data.removeFirst();
    }

    /**
     * Fetch the value at the head of the queue.
     *
     * @pre The queue is not empty
     * @post The element at the head of the queue is returned
     * 
     * @return Reference to the first value of the queue.
     */
    public E get()
    {
        return data.getFirst();
    }

    /**
     * Determine the number of elements within the queue.
     *
     * @post Returns the number of elements in the queue
     * 
     * @return The number of elements within the queue.
     */
    public int size()
    {
        return data.size();
    }

    /**
     * Remove all the values from the queue.
     *
     * @post Removes all elements from the queue
     */
    public void clear()
    {
        data.clear();
    }

    /**
     * Determine if the queue is empty.
     *
     * @post Returns true iff the queue is empty
     * 
     * @return True iff the queue is empty.
     */
    public boolean isEmpty()
    {
        return data.isEmpty();
    }

    public Iterator<E> iterator()
    {
        return data.iterator();
    }

    /**
     * Construct a string representation of the queue.
     *
     * @post Returns string representation of queue
     * 
     * @return String representing the queue.
     */
    public String toString()
    {
        StringBuffer s = new StringBuffer();
        s.append("<QueueList:");
        Iterator li = data.iterator();
        while (li.hasNext())
        {
            s.append(" "+li.next());
        }
        s.append(">");
        return s.toString();
    }
}
