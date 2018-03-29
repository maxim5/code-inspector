// Implementation of priority queues/heaps using binary trees.
// (c) 1998, 2001 duane a. bailey
package structure5;
import java.util.Iterator;
import java.util.Random;

/**
 * An implementation of a priority queue using skew heaps.  Skew heaps
 * allow one to construct heaps dynamically without explictly balancing
 * the heaps.  Main operation is a merge. Most operations execute in
 * amortized logarithmic time, but individual operations may take linear
 * time to execute in the worst case.
 *
 * <P>
 * Example usage:
 * <P>
 * To print out a list of programmers sorted by age we could use the following:
 * <pre>
 * public static void main(String[] argv){
 *      //initialize a new fib heap
 *      SkewHeap programmers = new {@link #SkewHeap()};
 *
 *      //add programmers and their ages to heap
 *      //ages current of 7/22/2002
 *      //add programmers and their ages to heap
 *      //ages current of 7/22/2002
 *        programmers.{@link #add(Comparable) add(new ComparableAssociation(new Integer(22), "Evan"))};
 *      programmers.add(new ComparableAssociation(new Integer(19), "Chris"));
 *      programmers.add(new ComparableAssociation(new Integer(20), "Shimon"));
 *      programmers.add(new ComparableAssociation(new Integer(21), "Diane"));
 *      programmers.add(new ComparableAssociation(new Integer(21), "Lida"));    
 *      programmers.add(new ComparableAssociation(new Integer(20), "Rob"));     
 *      programmers.add(new ComparableAssociation(new Integer(20), "Sean"));    
 *
 *      //print out programmers 
 *      while(!programmers.{@link #isEmpty()}){
 *          ComparableAssociation p = (ComparableAssociation)programmers.{@link #remove()};
 *          System.out.println(p.getValue() + " is " + p.getKey() + " years old.");
 *      }
 * }
 * </pre>
 * @version $Id: SkewHeap.java 35 2007-08-09 20:38:38Z bailey $
 * @author, 2001 duane a. bailey
 */ 
public class SkewHeap<E extends Comparable<E>> implements MergeableHeap<E>
{
    /**
     * The root of the skew heap.
     */
    protected BinaryTree<E> root;

    protected final BinaryTree<E> EMPTY = new BinaryTree<E>();
    /**
     * The number of nodes within heap.
     */
    protected int count;

    /**
     * Constructs an empty priority queue.
     *
     * @post creates an empty priority queue
     */
    public SkewHeap()
    {
        root = EMPTY;
        count = 0;
    }

    /**
     * Fetch lowest valued (highest priority) item from queue.
     *
     * @pre !isEmpty()
     * @post returns the minimum value in priority queue
     * 
     * @return The smallest value from queue.
     */
    public E getFirst()
    {
        return root.value();
    }

    /**
     * Returns the minimum value from the queue.
     *
     * @pre !isEmpty()
     * @post returns and removes minimum value from queue
     * 
     * @return The minimum value in the queue.
     */
    public E remove()
    {
        E result = root.value();
        root = merge(root.left(),root.right());
        count--;
        return result;  
    }

    /**
     * Add a value to the priority queue.
     *
     * @pre value is non-null comparable
     * @post value is added to priority queue
     * 
     * @param value The value to be added.
     */
    public void add(E value)
    {
        BinaryTree<E> smallTree = new BinaryTree<E>(value,EMPTY,EMPTY);
        root = merge(smallTree,root);
        count++;
    }

    /**
     * Determine the size of the queue.
     *
     * @post returns number of elements within queue
     * 
     * @return The number of elements within the queue.
     */
    public int size()
    {
        return count;
    }

    /**
     * Remove all the elements from the queue.
     *
     * @post removes all elements from queue
     */
    public void clear()
    {
        root = EMPTY;
    }

    /**
     * Determine if the queue is empty.
     *
     * @post returns true iff no elements are in queue
     * 
     * @return True if the queue is empty.
     */
    public boolean isEmpty()
    {
        return size() == 0;
    }
    
    /**
     * Merge this heap with another
     *
     * @param otherHeap Heap to be merged with this heap, otherHeap
     * is destroyed by this operation;
     * @post the two heaps are merged and otherHeap is destroyed
     */
    public void merge(MergeableHeap<E> otherHeap){
        Assert.pre(otherHeap instanceof SkewHeap, 
                   "otherHeap must be instance of SkewHeap");
        SkewHeap<E> that = (SkewHeap<E>)otherHeap;
        root = merge(this.root, that.root);
        that.root = null;
        this.count += that.count;
    }

    protected static <E extends Comparable<E>>
        BinaryTree<E> merge(BinaryTree<E> left, BinaryTree<E> right)
    {
        if (left.isEmpty()) return right;
        if (right.isEmpty()) return left;
        E leftVal = left.value();
        E rightVal = right.value();
        BinaryTree<E> result;
        if (rightVal.compareTo(leftVal) < 0)
        {
            result = merge(right,left);
        } else {
            result = left;
            // assertion left side is smaller than right
            // left is new root
            if (result.left().isEmpty())
            {
                result.setLeft(right);
            } else {
                BinaryTree<E> temp = result.right();
                result.setRight(result.left());
                result.setLeft(merge(temp,right));
            }
        }
        return result;
    }

    /**
     * Construct a string representation of the heap.
     *
     * @post returns string representation of heap
     * 
     * @return The string representing the heap.
     */
    public String toString()
    {
        if (root.isEmpty()) return "<SkewHeap: >";
        StringBuffer sb = new StringBuffer();
        sb.append("<SkewHeap:");
        if (!root.isEmpty()) {
            Iterator i = root.iterator();
            while (i.hasNext())
            {
                sb.append(" "+i.next());
            }
        }
        return sb+">";
    }

    public static void main(String[] argv){
        int s1 = 0, s2 = 0, s3 = 1, max = 0;
        try{
            while(s3 != 198){
                Random r = new Random();
                max = 0;
                s1 = 0;
                s2 = 0;
                s3 = 0;
                SkewHeap<Integer> t1 = new SkewHeap<Integer>();
                SkewHeap<Integer> t2 = new SkewHeap<Integer>();
                max = r.nextInt(100);
                for(int i=0; i < max; i++){
                    t1.add(new Integer(r.nextInt(1000))); 
                }
                max = r.nextInt(100);
                for(int i=0; i < max; i++){
                    t2.add(new Integer(r.nextInt(1000)));
                }
                
                //System.out.println(t1);
                //System.out.println(t2);
                s1 = t1.size();
                s2 = t2.size();
                t1.merge(t2);
                s3 = t1.size();
                //System.out.println(t1);
                Assert.condition(s1+s2==s3,"Trees merge with right sizes.");
                System.out.println(s1 + " : " + s2 + " : " + s3);
            }
        } catch (NullPointerException e) {
            System.out.println(s1 + " : " + s2 + " : " + s3);
        }
    }
}
