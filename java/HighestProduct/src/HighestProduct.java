// Given an array of integers, find the highest product you can get from three of the integers.

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

public class HighestProduct {
    static class Test {
        static void test(int[] ints, int expected){
            ArrayList<Integer> input = new ArrayList<>();
            for(Integer i : ints) {
                input.add(i);
            }
            int actualOutput = (new HighestProduct()).highestProduct(input);
            if(actualOutput == expected) {
                System.out.println("ok");
            } else {
                for(Integer i : input) {
                    System.out.printf("%d ", i);
                }
                System.out.printf(": %d != %d", expected, actualOutput);
            }
        }
    }

    public static void main(String args[]){
        Test.test(new int[]{1,2,3}, 6);
        Test.test(new int[]{1,2,3, 4}, 24);
        Test.test(new int[]{1,2,-3}, -6);
        Test.test(new int[]{1,0,3}, 0);
        Test.test(new int[]{-1,-2,3}, 6);
        Test.test(new int[]{1,2,3, -4, -3}, 36);
        Test.test(new int[]{1,2,3, -5, -6}, 90);
        Test.test(new int[]{1,2,3, -5, 6}, 36);
        Test.test(new int[]{1,2,3, -6, -6, -6}, 6 * 6 * 3);
        Test.test(new int[]{-1,2,3, -6, -6, -6}, 6 * 6 * 3);

    }



    int highestProduct(ArrayList<Integer> ints){

        int[] pos = {Integer.MIN_VALUE, Integer.MIN_VALUE, Integer.MIN_VALUE}; //ordered: [largest, secondlargest, thirdlargest]
        int[] neg = {Integer.MAX_VALUE, Integer.MAX_VALUE}; //ordered: [smallest, secondsmallest]

        //init
        for(Integer i: ints){
            if(i >= pos[0]) {
                pos[2] = pos[1];
                pos[1] = pos[0];
                pos[0] = i;
            } else if (i >= pos[1]){
                pos[2] = pos[1];
                pos[1] = i;
            } else if (i > pos[2]){
                pos[2] = i;
            }

            if(i <= neg[0]) {
                neg[1] = neg[0];
                neg[0] = i;
            } else if (i < neg[1]) {
                neg[1] = i;
            }

        }


        int optionA = pos[0] * pos[1] * pos[2];
        int optionB = pos[0] * neg[0] * neg[1];
        return Integer.max(optionA, optionB);
    }
}
