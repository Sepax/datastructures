
import java.util.*;

// A priority queue.
public class PriorityQueue<E> {
	private ArrayList<E> heap = new ArrayList<>();
	private Comparator<E> comparator;
	private Map<E, Integer> hashMap = new HashMap<>();

	public PriorityQueue(Comparator<E> comparator) {
		this.comparator = comparator;
	}

	// Returns element at index i.
	public E get(int index) {
		return heap.get(index);
	}

	// Returns the size of the priority queue.
	public int size() {
		return heap.size();
	}

	// Adds an item to the priority queue.
	// Complexity: O(log n)
	public void add(E elem) {
		// Add element to the hashMap and to the end of the heap.
		hashMap.put(elem, heap.size());
		heap.add(elem);

		// Fix invariant by sifting up the new element.
		// (this also updates the hashMap)
		siftUp(heap.size() - 1);

		assert invariant() : "Invariant failed after add()";
	}

	// Update Item in the priority queue.
	// Complexity: O(log n)
	public void update(E oldElem, E newElem) {
		// Check if oldElem is in the priority queue.
		if (!hashMap.containsKey(oldElem)) {
			throw new NoSuchElementException();
		}

		// Check if newElem is already in the priority queue.
		if (oldElem.equals(newElem)) {
			return;
		}

		// Update hashMap and heap.
		int index = hashMap.get(oldElem);
		pqUpdate(newElem, index);

		// Fix invariant by sifting up or down
		// depending on if newElem is greater or less than oldElem.
		if (comparator.compare(oldElem, newElem) > 0) {
			siftUp(index);
		} else {
			siftDown(index);
		}

		assert invariant() : "Invariant failed after update()";
	}

	// Returns the smallest item in the priority queue.
	// Throws NoSuchElementException if empty.
	// Complexity: O(1)
	public E minimum() {
		if (size() == 0) {
			throw new NoSuchElementException();
		}
		return heap.get(0);
	}

	// Removes the smallest item in the priority queue.
	// Throws NoSuchElementException if empty.
	// Complexity: O(log n)
	public void deleteMinimum() {
		if (size() == 0) {
			throw new NoSuchElementException();
		}

		// Overwrite root with last element, then remove last element.
		pqUpdate(heap.get(heap.size() - 1), 0);
		pqDelete(heap.size() - 1);

		// Fix invariant by sifting down root. (which was last element)
		if (!heap.isEmpty()) {
			siftDown(0);
		}

		assert invariant() : "Invariant failed after deleteMinimum()";
	}

	// Sifts a node up.
	// siftUp(index) fixes the invariant if the element at 'index' may
	// be less than its parent, but all other elements are correct.
	// Complexity: O(log n)
	private void siftUp(int index) {
		E value = heap.get(index);

		// Stop when the node is at the root, or earlier if possible.
		while (index > 0) {
			int parentIndex = parent(index);
			E parentValue = heap.get(parentIndex);

			// Check invariant.
			if (comparator.compare(value, parentValue) >= 0) {
				break;
			}

			// If it's not correct, swap with parent.
			pqUpdate(value, parentIndex);
			pqUpdate(parentValue, index);
			index = parentIndex;
		}
	}

	// Sifts a node down.
	// siftDown(index) fixes the invariant if the element at 'index' may
	// be greater than its children, but all other elements are correct.
	// Complexity: O(log n)
	private void siftDown(int index) {
		E value = heap.get(index);

		// Stop when the node is a leaf.
		while (leftChild(index) < heap.size()) {
			int left = leftChild(index);
			int right = rightChild(index);

			// Work out whether the left or right child is smaller.
			// Start out by assuming the left child is smaller...
			int child = left;
			E childValue = heap.get(left);

			// ...but then check in case the right child is smaller.
			// (We do it like this because maybe there's no right child.)
			if (right < heap.size()) {
				E rightValue = heap.get(right);
				if (comparator.compare(childValue, rightValue) > 0) {
					child = right;
					childValue = rightValue;
				}
			}

			// If the child is smaller than the parent,
			// carry on downwards.
			if (comparator.compare(value, childValue) > 0) {
				pqUpdate(childValue, index);
				index = child;
			} else
				break;
		}

		pqUpdate(value, index);
	}

	// Helper functions for calculating the children and parent of an index.
	private final int leftChild(int index) {
		return 2 * index + 1;
	}

	private final int rightChild(int index) {
		return 2 * index + 2;
	}

	private final int parent(int index) {
		return (index - 1) / 2;
	}

	// Update priority queue & hash map
	// Complexity: O(1)
	private void pqUpdate(E value, int index) {
		hashMap.put(value, index);
		heap.set(index, value);
	}

	// Delete from priority queue & hash map
	// Complexity: O(1)
	private void pqDelete(int index) {
		hashMap.remove(heap.get(index));
		heap.remove(index);
	}

	// Heap invariant
	// Complexity: O(n)
	private boolean invariant() {
		for (int i = 0; i < heap.size(); i++) {
			// Check if right or left child is smaller than parent
			E node = heap.get(i);
			int leftIndex = leftChild(i);
			int rightIndex = rightChild(i);

			boolean leftSmaller = leftIndex < heap.size() && comparator.compare(node, heap.get(leftIndex)) > 1;
			boolean rightSmaller = rightIndex < heap.size() && comparator.compare(node, heap.get(rightIndex)) > 1;

			if (leftSmaller || rightSmaller) {
				return false;
			}

			// Check if hashmap is correct
			if (hashMap.get(node) != i) {
				return false;
			}
		}

		return true;
	}

}
