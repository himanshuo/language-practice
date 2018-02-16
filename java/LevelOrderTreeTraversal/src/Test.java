import java.util.ArrayList;

public class Test {

    //[0 1 2 x x 3 4]
//        0
//      1   2
//        3   4

    public Node generateTree(ArrayList<String> nodesAsArrayList, int i) {
        if (i >= nodesAsArrayList.size()) return null;
        String cur = nodesAsArrayList.get(i);
        if(!cur.equalsIgnoreCase("x")) {
            Node current = new Node();
            current.val = Integer.parseInt(nodesAsArrayList.get(i));
            current.left = generateTree(nodesAsArrayList, i * 2 + 1);
            current.right = generateTree(nodesAsArrayList, i * 2 + 2);
            return current;
        }
        return null;
    }

    public void test(String input, String expectedOutput) {
        String[] nodesAsArray = input.split("\\w");
        ArrayList<String> nodesAsArrayList = new ArrayList<>();
        for(String i: nodesAsArray) {
            if(!i.equals("") && !i.equals(" ")){
                nodesAsArrayList.add(i);
            }
        }
        Node head = generateTree(nodesAsArrayList, 0);
        LevelOrderTreeTraversal lott = new LevelOrderTreeTraversal();
        ArrayList<Integer> actual = lott.traverseIterative(head);
        ArrayList<Integer> expected = new ArrayList<>();
        for(String i: expectedOutput.split("\\w")) {
            if(!i.equals("") && !i.equals(" ")) {
                expected.add(Integer.parseInt(i));
            }
        }

        if(actual.equals(expected)) {
            System.out.println("ok");
        } else {
         System.out.printf("Input:[%s]", input);
         System.out.printf("Expected:[%s]", expectedOutput);
         String actualOutput = "";
         for(Integer a: actual){
             actualOutput += "" + a + " ";
         }
         System.out.printf("Actual:[%s]", actualOutput);
         System.out.println("");

        }

    }

}
