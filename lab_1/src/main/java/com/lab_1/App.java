package com.lab_1;

import java.util.Arrays;
import java.util.Random;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) {
        int[] randomArray = { 44, 32, 56, 22, 3, 5, 66, 6, 6, 2, 1, 6, 5, 8, 9, 3, 4, 5, 6, 7, 8, 9, 0, 12, 43 };
        System.out.println("Original Array:   " + Arrays.toString(randomArray));

        int[] insertArray = Arrays.copyOf(randomArray, randomArray.length);
        insertionSort(insertArray);
        System.out.println("Insertion Sorted: " + Arrays.toString(insertArray));

        int[] quickArray = Arrays.copyOf(randomArray, randomArray.length);
        quickSort(quickArray);
        System.out.println("Quick Sorted:     " + Arrays.toString(quickArray));
    }

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

    // Quicksort.

    public static void quickSort(int[] array) {
        quickSort(array, 0, array.length - 1);
    }

    // Quicksort part of an array
    private static void quickSort(int[] array, int begin, int end) {
        if (begin >= end) {
            return;
        }
        int pivot = partition(array, begin, end);

        quickSort(array, begin, pivot - 1);
        quickSort(array, pivot + 1, end);
    }

    // Partition part of an array, and return the index where the pivot
    // ended up.
    private static int partition(int[] array, int begin, int end) {
        Random random = new Random();
        int pivotIndex = random.nextInt(begin, end);
        int pivot = array[pivotIndex];

        while (end > begin) {

            while (array[begin] < pivot) {
                begin++;
            }

            while (array[end] > pivot) {
                end--;
            }

            swap(array, begin, end);
            begin++;
            end--;
        }
        System.out.println("begin: " + begin);
        System.out.println("end: " + end);
        System.out.println(Arrays.toString(array));
        swap(array, pivotIndex, end);

        return end;
    }

    // Swap two elements in an array
    private static void swap(int[] array, int i, int j) {
        int x = array[i];
        array[i] = array[j];
        array[j] = x;
    }

    // Mergesort.

    public static int[] mergeSort(int[] array) {
        throw new UnsupportedOperationException();
    }

    // Mergesort part of an array
    private static int[] mergeSort(int[] array, int begin, int end) {
        throw new UnsupportedOperationException();
    }

    // Merge two sorted arrays into one
    private static int[] merge(int[] left, int[] right) {
        throw new UnsupportedOperationException();
    }
}
