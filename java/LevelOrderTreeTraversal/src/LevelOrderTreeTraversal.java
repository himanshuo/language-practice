import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

public class LevelOrderTreeTraversal {

//https://stackoverflow.com/a/19330833


    public static void main(String args[]){
        Test t = new Test();
        t.test("", "");
        t.test("1", "1");
        t.test("1 x x", "1");
        t.test("1 2 3", "1 2 3");
        t.test("1 2 x", "1 2");
        t.test("1 x 3", "1 3");
        t.test("1 2 3 x 5 6 7", "1 2 3 5 6 7");
        t.test("1 2 3 4 x 6 7", "1 2 3 4 6 7");
        t.test("1 2 3 4 5 x 7", "1 2 3 4 5 7");
        t.test("1 2 3 4 5 6 x", "1 2 3 4 5 6");
    }

// Process all nodes of a tree by depth: first the root, then the children of the root, etc

    public ArrayList<Integer> traverseIterative(Node head){
        Queue<Node> q = new LinkedList<>();
        q.offer(head);
        Node cur = null;
        ArrayList<Integer> out = new ArrayList<>();
        while(q.peek() != null) {
            cur = q.poll();
            out.add(cur.val);
            if(cur.left != null) q.offer(cur.left);
            if(cur.right != null) q.offer(cur.right);
        }
        return out;
    }
}

