package com.lab_1;

import java.util.Random;

/**
 * Hello world!
 *
 */
public class Lab1 {

    private static Random random = new Random();

    /** Sorting algorithms **/

    // Insertion sort.

    public static void insertionSort(int[] array) {
        for (int i = 1; i < array.length; i++) {
            int j = i;
            while (j > 0 && array[j - 1] > array[j]) {
                swap(array, j - 1, j);
                j--;
            }
        }
    }

    // QuickSort.
    public static void quickSort(int[] array) {
        quickSort(array, 0, array.length - 1);
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int left, int right) {
        // Check if there are any elements to sort
        if (left < right) {
            // Choose a pivot index and partition the array around it
            int pivotIndex = partition(array, left, right);
            // Recursively sort the left and right partitions
            quickSort(array, left, pivotIndex - 1);
            quickSort(array, pivotIndex + 1, right);
        }
    }

    // Partition part of an array, and return the index where the pivot
    // ended up.
    private static int partition(int[] array, int left, int right) {
        // Choose a random pivot index
        int pivotIndex = left + random.nextInt(right - left + 1);
        int pivotValue = array[pivotIndex];
        // Move the pivot element to the end of the array
        swap(array, pivotIndex, right);
        // Keep track of the index where elements less than the pivot value will be
        // stored
        int storeIndex = left;
        // Iterate over the array and move elements less than the pivot value to the
        // left partition
        for (int i = left; i < right; i++) {
            if (array[i] < pivotValue) {
                swap(array, i, storeIndex);
                storeIndex++;
            }
        }
        // Move the pivot element to its final place
        swap(array, storeIndex, right);
        // Return the index of the pivot element
        return storeIndex;
    }

    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
    }

    // Mergesort.
    public static int[] mergeSort(int[] array) {
        return mergeSort(array, 0, array.length - 1);
    }

    // Recursive method that implements the merge sort algorithm.
    private static int[] mergeSort(int[] array, int begin, int end) {
        // Base case: If the array has only one element, return it.
        if (begin >= end) {
            int[] baseCase = { array[begin] };
            return baseCase;
        }

        // Divide the array into two halves and recursively sort each half.
        int mid = (begin + end) / 2;
        int[] left = mergeSort(array, begin, mid);
        int[] right = mergeSort(array, mid + 1, end);

        // Merge the sorted halves and return the result.
        return merge(left, right);
    }

    // This is the method that merges two sorted arrays into a single sorted array.
    private static int[] merge(int[] left, int[] right) {
        int[] result = new int[left.length + right.length];
        int i = 0, j = 0, k = 0;

        // Merge the two arrays by comparing the elements and adding them to the result
        // array.
        while (i < left.length && j < right.length) {
            if (left[i] < right[j]) {
                result[k++] = left[i++];
            } else {
                result[k++] = right[j++];
            }
        }

        // Add any remaining elements from the left array.
        while (i < left.length) {
            result[k++] = left[i++];
        }

        // Add any remaining elements from the right array.
        while (j < right.length) {
            result[k++] = right[j++];
        }

        // Return the sorted array.
        return result;
    }

}
