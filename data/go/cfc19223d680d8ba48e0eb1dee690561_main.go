package main

import ()

type (
	Node struct {
		Value    String
		Next     *Node
		Previous *Node
	}

	ThreeStacks []byte

	PushPopMinStack struct {
		stack []byte
		min   int
	}

	StackOfStacks [][]byte

	Stack []byte
)

const ()

var ()

func init() {

}

func main() {

}

// 1  Arrays and Strings

// 1.1
// Implement an algorithm to determine if a string has all unique characters.
// What if you can't use additional data structures?
func UniqueChars(input string) bool {

	// nÂ˛
	var l = len(input)
	for i := 0; i < l; i++ {
		for j := i + 1; j < l; j++ {
			if input[i] == input[j] {
				return false
			}
		}
	}
	return true

	// n
	// dict := make(map[rune]bool)
	// for _, c := range input {
	// 	if _, exists := dict[c]; exists {
	// 		return false
	// 	}
	// 	dict[c] = true
	// }
	// return true

}

// 1.2
// Implement a function to reverse the characters of a string
// more difficult in C
func ReverseChars(input string) string {

}

// 1.3
// Given two strings, write a method to decide if one is a permutation of the other
func IsPermutation(a, b string) bool {

}

// 1.4
// Write a method to replace all spaces in a string with %20.  You may assume
// that the string has sufficient space at the end of the string to hold the
// additional characters and that you are given the true length of the string.
// Example:
// "Mr John Smith    "
// "Mr%20John%20Smith"
func EscapeSpaces(input string) string {

}

// 1.5
// Implement a method to perform basic string compression using the counts of
// repeated characters (delta compression).  For example the string "aabcccccaaa"
// would become "a2b1c5a3".  If the compressed string would not become smaller
// than the original string, your method should return the original string
func DeltaCompressString(input string) string {

}

// 1.6
// Given an image represents an NxN matrix, where each pixel in the image is
// 4 bytes, write a method to rotate the image by 90 degress.  Can you do this in place?
func RotateMatrix90Degrees(input [][]uint) [][]uint {

}

// 1.7
// Write an algorithm such that if an element in an MxN matrix is 0, it's entire row
// and column are set to 0
func ZeroRowAndColumn(input [][]byte) [][]byte {

}

// 1.8
// Assume you have a method isSubstring which checks if one word is a substring
// of another.  Given two strings, s1 and s2, write code to check if s2 is a
// rotation of s1 using only one call to isSubstring (e.g., "waterbottle" is a
// rotation of "erbottlewat")
func IsRotationOf(a, b string) bool {

}

// 2 Linked Lists

// 2.1
// Write code to remove duplicates from an unordered linked list
// How would you solve this problem without a temporary buffer?
func RemoveDuplicates(input *Node) *Node {

}

// 2.2
// Implement an algorithm to find the kth to last element of a singly linked list
func KthElementFromEnd(node *Node) *Node {

}

// 2.3
// Implement an algorithm to delete a node in the middle of a singly linked list,
// given only access to that node.
// Example:
// Input: the node c from the linked list a->b->c->d->e
// Result: nothing is returned, but the new linked list looks like a->b->d->e
func RemoveNode(node *Node) *Node {

}

// 2.4
// Write code to partition a linked list around a value x, such that all nodes less
// than x come before all nodes greater than or equal to x.
func PartitionLinkedListByNode(node *Node) *Node {

}

// 2.5
// You have two numbers represented by a linked list, where each node contains a
// single digit.  The digits are stored in reverse order, such that the 1's digit
// is at the head of the list.  Write a function that adds two numbers and returns
// the sum as a linked list.
//
// Example:
// Input: (7->1->6) + (5->9->2). That is 617 + 295
// Output: (2->1->9) That is 219.
//
// Suppose the digits are stored in forward order, solve the above problem.
//
// Example:
// Input: (6->1->7) + (2->9->5). That is 617 + 295
// Output: (9->1->2) That is 912.
func LinkedListReverseAddition(node *Node) *Node {

}
func LinkedListAddition(node *Node) *Node {

}

// 2.6
// Given a circular linked list, implement an algorithm which returns the node
// at the beginning of the loop.
//
// Definition: A circular linked list is a list in which a node's next pointer points
// to an earlier node, so as to make a loop in the linked list.
//
// Example:
// (a->b->c->d->e->c)
// Output: c
func FirstNodeOfCircularLinkedList(node *Node) *Node {

}

// 2.7
// Implement a function to check if a linked list is a palindrome.
func IsPalindrome(node *Node) bool {

}

// 3 Stacks and Queues

// 3.1
// Describe how you could use a single array to implement three stacks
func CreateThreeStacksFromSingleArray() *ThreeStacks {

}

// 3.2
// how would you design a stack which, in addition to push and pop, also has
// a function min which returns the minimum element?  Push, pop, and min should
// all operate in O(1) time.
func CreatePushPopMinStack() *PushPopMinStack {

}

// 3.3
// Imagine a (literal) stack of plates.  If the stack gets too high, it might topple.
// Therefore, in real life, we would likely start a new stack owhen the previous stack
// exceeds some threshold.  Implement a data structure SetOfStacks that mimics this.
// SetOfStacks should be composed of several stacks and should create a new stack once
// the previous one exceeds capacity.  Push() and Pop() should behave identically to a
// single stack (that is, Pop() should return the same values as it would if there were
// just a single stack).
//
// FOLLOWUP:
// Implement a function PopAt(index int) which performs a pop opearation on a specific sub-stack.
func CreateStackOfStacks() *StackOfStacks {

}

// 3.4
// In a classic problem of the Towers of Hanoi, you have 3 towers and N disks of different sizes
// which can slide onto any tower.  The puzzle starts with disks sorted in ascending order of size
// from top to bottom (i.e., each disk sits on top of an even larger one).  You have the following
// constraints:
//   - Only one disk can be moved at a time.
//   - A disk is slid off the top of one tower onto the next tower.
//   - A disk can only be placed on top of a larger disk.
// Write a program to move the disks from the first tower to the last using stacks.
func MoveFirstTowerToLastTowerOfHanoiUsingStacks() {

}

// 3.5
// Implement a queue using two stacks
func CreateQueueFromTwoStacks() *TwoStackQueue {

}

// 3.6
// Write a program to sort a stack in ascending order (with biggest items on top).
// you may use additional stacks to hold items, but you may not copy the elements
// into any other structure (such as an array).  The stack supports the following
// operations: Push(), Pop(), Peek(), and IsEmpty()
func SortStackInAscendingOrder(s *Stack) {

}

// 3.7
// An animal shelter holds only dogs and cats, and operates on a strictly "first in,
// first out" basis.  People must adopt either the "oldest" (based on arrival time) of
// all animals at the shelter, or they can select whether they would prefer a dog or
// a cat (and will receive the oldest animal of that type).  They cannot select which
// specific animal they would like.  Create the data structures to maintain this system
// and implement the operations such as Enqueue(), DequeueAny(), DequeueDog(), and
// DequeueCat().  You may use the built-in LinkedList data structure.
func CreateAnimalShelter() *AnimalShelter {

}

// 4 Trees and Graphs

// 4.1
// Implement a function to check if a binary tree is balanced.  For the purpose of this
// question a balanced tree is defined to be a tree such that the heights of the two
// subtrees of any node never differ by more than one.
func IsBalancedTree(tree *Node) bool {

}

// 4.2
// Given a directed graph, design an algorithm to find out whether there is a route
// between two nodes
func HasRoute(tree *Node, a *Node, b *Node) bool {

}

// 4.3
// Given a sorted (increasing order) array, write an algorithm to create a binary
// search tree with minimal height.
func ArrayToBinarySearchTree(input []string) *Node {

}

// 4.4
// Given a binary tree, design an algorithm which creates a linked list of all the
// nodes at each depth (e.g., if you have a tree with depth D, you'll have D linked lists).
func BinaryTreeToArrayOfLinkedListsPerDepth(tree *Node) []*Node {

}

// 4.5
// Implement a function to check if a binary tree is a binary search tree
func IsBinarySearchTree(tree *Node) bool {

}

// 4.6
// Write an algorithm to find the 'next' node (i.e., in-order sucessor) of a given node
// in a binary search tree.  You may assume that each node has a link to its parent.
func NextNodeInBinarySearchTree(tree *Node, node *Node) *Node {

}

// 4.7
// Design an algorithm and write code to find the first common ancestor of two nodes
// in a binary tree.  Avoid storing additional nodes in a data structure.
// NOTE: This is not necessarily a binary search tree.
func FindCommonAncestorOfTwoNodes(tree *Node, a *Node, b *Node) *Node {

}

// 4.8
// You have two very large binary trees: T1, with millions of nodes, and T2, with
// hundreds of nodes.  Create an algorithm to decide if T2 is the subtree of T1
//
// A tree T2 is a subtree of T1 if there exists a node n in T1 such that the subtree
// of n is identical to T2.  That is, if you cut off the tree at node n, the two trees
// would be identical.
func IsSubTreeOf(T1 *Node, T2 *Node) bool {

}

// 4.9
// You are given a binary tree in which each node contains a value.  Design an algorithm
// to print all paths which sum to a given value.  Note that a path can start or end anywhere
// in the tree.
func FindAllPathsBySum(tree *Node, sum int) {

}

// 5 Bit Manipulation

// 5.1
// You are given two 32-bit numbers, N and M, and two bit positions, i and j.  Write
// a method to insert M into N such that M starts at bit j and ends at bit i.  You can
// assume that the bits j through i have enough space to fit all of M.  That is, if
// M = 10011, you can assume that ther eare at least 5 bits between j and i.  You
// would not, for example, have j = 3 and i = 2, because M could not fully fit
// between bit 3 and bit 2.
//
// EXAMPLE:
// Input:  N = 10000000000, M = 10011, i = 2, j = 6
// Output: N = 10001001100
func BitSplice(N uint32, M uint32, i int, j int) uint32 {

}

// 5.2
// Given a real number between 0 and 1 (e.g. 0.72) that is passed in as a double
// print the binary representation.  If the number cannot be represented accurately
// in binary with at most 32 characters print 'ERROR'
func PrintDoubleAsBinaryCharacters(n float64) []byte {

}

// 5.3
// Given a positive integer, print the next smallest and the next largest number
// that have the same number of 1 bits in their binary representation
func NextSmallestAndLargestIntegerWithSameNumberOfBitsSet(n uint) (uint, uint) {

}

// 5.4
// Explain what the following code does:
// (n & (n - 1)) == 0
func IsPowerOfTwo(n uint) bool {
	return (n & (n - 1)) == 0
}

// 5.5
// Write a function to determine the nubmer of bits required to convert integer A
// to integer B
// EXAMPLE:
// Input: 31, 14
// Output: 2
func BitsRequiredToConvertInteger(A uint, B uint) int {

}

// 5.6
// Write a program to swap odd and even bits in an integer with as few instructions
// as possible (e.g., bit 0 and bit 1 are swapped, bit 2 and bit 3 are swapped, and
// so on).
func SwapAdjacentBits(n uint) uint {

}

// 5.7
// An array A contains all the integers from 0 to n, except for one number which is
// missing.  In this problem, we cannot access an entire integer in A with a single
// operation.  The elements of A are represented in binary, and the only operation
// we can use to access them is "fetch the jth bit of A[i]," which takes constant
// time. Write code to find the missing integer.  Can you do it in O(n) time?
func FindMissingIntegerInRange(A []uint) uint {

}

// 5.8
// A monochrome screen is stored as a single array of bytes, allowing eight consecutive
// pixels to be stored in one byte.  The screen has width w, where w is divisible by 8
// (that is, no byte will be split across rows).  The height of the screen is, of course,
// can be derived from the length of the array and the width.  Implement a function
// which draws a horizontal line from (x1, y) to (x2, y).
func DrawHorizontalLine(screen []byte, width int, x1 int, x2 int, y int) []byte {

}

// 6 Brain Teasers

// 6.1
// You have 20 bottles of pills.  19 bottles have 1.0 gram pills, but one has pills
// of weight 1.1 grams.  Given a scale that provides an exact measurement, how would
// you find the heavy bottle?  You can only use the scale once.

// 6.2
// There is an 8x8 chess board in which two diagonally opposite corners have been cut
// off.  You are given 31 dominos, and a single domino can cover exactly two squares.
// Can you use the 31 dominos to cover the entire board?  Prove your answer (by providing
// an example or showing why it's impossible).

// 6.3
// You have a five-quart jug, a three-quart jug, and an unlimited supply of water (but
// no measuring cups).  How would you come up with exactly four quarts of water?  Note
// that the jugs are oddly shaped, such that filling up exactly "half" of the jug would
// be impossible.

// 6.4
// A bunch of people are living on an island, when a visitor comes with a strange order:
// all blue-eyed people must leave the island as soon as possible.  There will be a flight
// out at 8:00pm every evening.  Each person can see everyone else's eye color, but they do
// not know their own (nor is anyone allowed to tell them).  Additionally, they do not know
// how many people have blue eyes, although they do know that at least one person does.
// How many days will it take the blue-eyed people to leave.

// 6.5
// There is a building of 100 floors.  If an egg drops from the Nth floor or above, it
// will break.  If it's dropped from any floor below, it will not break.  You're given
// two eggs.  Find N, while minimizing the number of drops for the worst case.

// 6.6
// There are 100 closed lockers in a hallway.  A man begins by opening all 100 lockers.
// Next, he closes every second locker.  Then, on his third pass, he toggles every third
// locker (closes it if it is open or opens it if it is closed).  This process continues
// for 100 passes, such that on each pass i, the man toggles every ith locker.  After his
// 100th pass in the hallway, in which he toggles only locker #100, how many lockers are open?

// 7 Mathematics and Probability

// 7.1
// You have a basketball hoop and someone says that you can play one of two games.
// Game 1: You get one shot to make the hoop.
// Game 2: You get three shots and you have to make two of the three shots.
// If p is the probability of making a particular shot, for which values of p should you
// pick one game or the other.

// 7.2
// There are three ants on different vertices of a triangle.  What is the probability of
// collision (between any two or all of of them) if they start walking on the sides of the
// triangle?  Assume that each ant randomly picks a direction, with either direction being
// equally likely to be chosen, and that they walk at the same speed.
//
// Similarly, find the probability of collision with n ants on an n-vertex polygon.

// 7.3
// Given two lines on a Cartesian plane, determine whether the two lines would intersect.

// 7.4
// Write methods to implement the multiply, subtract, and divide operations for integers
// using only the add operator.
func MultiplyByAdding(a int, b int) int {

}
func SubtractByAdding(a int, b int) int {

}
func DivideByAdding(a int, b int) int {

}

// 7.5
// Given two squares on a two-dimensional plane, find a line that would cut these two
// squares in half.  Assume that the top and the bottom sides of the square run parallel
// to the x-axis.

// 7.6
// Given a two-dimensional graph with points on it, find a line which passes the most
// number of points.

// 7.7
// Design an algorithm to find the kth number such that the only prime factors are 3,5 and 7.

// 8 Object-oriented Design

// 8.1
// Design the data structures for a generic deck of cards.  Explain how you would subclass
// the data structures to implement blackjack.

// 8.2
// Imagine a call center with three levels of employees: respondent, manager, and director.
// An incoming telephone call must be first allocated to a respondent who is free.  If the
// respondent can't handle the call, he or shee must escalate the call to a manager.  If the
// manager is not free or not able to handle it, then the call should be escalated to a director.
// Design the classes and data structures for this problem.  Implement a method dispatchCall()
// which assigns a call to the first available employee.

// 8.3
// Design a musical jukebox using OO principles.

// 8.4
// Design a parking log using OO principles.

// 8.5
// Design the data structures for an online book reader system.

// 8.6
// Implement a jigsaw puzzle.  Design the data structures and explain an algorithm to solve
// the puzzle.  You can assume that you have a fitsWith() method which when passed two puzzle
// pieces, returns true if hte two pieces belong together.

// 8.7
// Explain how you would design a chat server.  In particular, provide details about the various
// backend components, classes, and methods.  What would be the hardest problems to solve?

// 8.8
// Othello is played as follows:  Each Othello piece is white on one side and black on the other.
// When a piece is surrounded by its opponents on both the left and the right sides, or both the
// top and bottom, it is said to be captured and its color is flipped.  On your turn, you must
// capture at least one of your opponent's pieces.  The game ends when either user has no more
// valid moves.  The win is assigned to the person with the most pieces.  Implement the OO
// design for Othello.

// 8.9
// Explain the data structures and algorithms that you would use to design an in-memory file-system.
// Illustrate with an example in code where possible.

// 8.10
// Design and implement a hash table which uses chaining (linked lists) to handle collisions.

// 9 Recursion and Dynamic Programming

// 9.1
// A child is running up a staircase with n steps, and can hop either 1 step, 2 steps, or 3 steps
// at a time.  Implement a method to count how many possible ways the child can run up the stairs.

// 9.2
// Imagine a robot sitting on the upper left corner of an X by Y grid.  The robot can only move in two
// directions: right and down.  How many possible paths are there for the robot to go from (0,0) to (X,Y)?
//
// FOLLOW UP:
// Imagine certain spots are "off limits," such that the robot cannot step on them.  Design an
// algorithm to find a path for the robot from the top left to the bottom right.

// 9.3
// A magic index in an array A[0...n-1] is defined to be an index such that A[i] = i.  Given
// a sorted array, write a method to find the magic index if one exists in A.
//
// FOLLOW UP:
// What if the values are not distinct?

// 9.4
// Write a method to return all subsets of a set.

// 9.5
// Write a method to compute all permutations of a string.

// 9.6
// Implement an algorithm to print all valid (e.g., properly opened and closed) combinations
// of n-pairs of parenthesis.
//
// Example:
// Input: 3
// Output: ((())), (()()), (())(), ()()()

// 9.7
// Implement the "paint fill" function that one might see on many image editing programs.
// That is given a screen (represented by a two-dimensional array of colors), a point,
// and a new color, fill the surrounding area until the color changes from the original color.

// 9.8
// Given an infinite number of quarters (25 cents), dimes (10 cents), nickels (5 cents), and
// pennies (1 cent), write code to calculate the number of ways of representing n cents.

// 9.9
// Write an algorithm to print all ways of arranging eight queens on an 8x8 chess board so
// that none of them share the same row, column, or diagonal.  In this case, "diagonal"
// means all diagonals, not just the two that bisect the board.

// 9.10
// You have a stack of n boxes, with widths w, heights h, and depths d.  The boxes
// cannot be rotated anc can only be stacked on top of one another if each box in the
// stack is strictly larger than the box above it in width, height, and depth.  Implement
// a method to build the tallest stack possible, where the height of a stack is the sum
// of the heights of each box.

// 9.11
// Given a boolean expression consisting  of the symbols 0, 1, &, |, and ^, and a desired
// boolean result value result, implement a function to count the number of ways of
// parenthesizing the expression such that it evauates to result.
//
// Example:
// Expression: 1^0|0|1
// Desired Result: false (0)
// Output: 2 ways.  1^((0|0)|1) and 1^(0|(0|1))

// 10 Scalability and Memory Limits

// 10.1
// Imagine you are building some sort of service that will be called by up to 1000 client
// applications to get simple end-of-day stock price information (open, close, high, low).
// You may assume that you already have hte data, and you can store it in any format you
// wish.  How would you design the client-facing service which provides the information
// to client applications?  You are responsible for the development, rollout, and ongoing
// monitoring and maintenance of the feed.  Describe the different methods you considered
// and why you would recommend your approach.  Your service can use any technologies you
// wish, and can distribute the information to the client applications in any mechanism
// you choose.

// 10.2
// How would you design the data structures for a very large social network like Facebook
// or LinkedIn?  Describe how you would design an algorithm to show the connection, or path,
// between two people (e.g., Me -> Bob -> Susan -> Jason -> You).

// 10.3
// Given an input file with four billion non-negative integers, provide an agorithm to
// generate an integer which is not contained in the file.  Assume you have 1GB of memory
// available for this task.
//
// FOLLOW UP:
// What if you have only 10MB of memory?  Assume that all values are distinct.

// 10.4
// You have an array with all the numbers from 1 to N, where N is at most 32,000.
// The array may have duplicate entries and you do not know what N is.  With only
// 4KB of memory available, how would you print all duplicate elements in the array?

// 10.5
// If you were designing a web crawler, how would you avoid getting into infinite loops?

// 10.6
// You have 10 billion URLs.  How would you detect duplicate documents?  In this case,
// assume that "duplicate" means the URLs are identical.

// 10.7
// Imagine a web server for a simplified search engine.  This system has 100 machines
// to respont to search queries, which may then call out using processSearch(string query)
// to another cluster of machines to actually get the result.  The machine which responds
// to a given query is chosen at random, so you can not guarantee that the same machine
// will always respond to the same request.  The method processSearch is very expensive.
// Design a caching mechanism for the most recent queries.  Be sure to explain how you
// would update the cache when data changes.
