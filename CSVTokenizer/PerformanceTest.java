import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class PerformanceTest {
    public static void main(String[] args) throws IOException {
        int n = Integer.parseInt(args[1]);
        String text = readFile(args[0]);
        ArrayList<Integer> res = new ArrayList<Integer>(n);
        for(int i=0; i < n; i++) {
            res.add(runTest(text));
        }

        int sum = 0;
        for(int i : res) {
            sum += i;
        }

        System.out.println("Avg: " + sum/n);
    }

    private static int runTest(String text) {
        long start = System.currentTimeMillis();
        for(int i=0; i<100000; i++) {
            CSVTokenizerCL.replaceUmlauts(text);
        }
        long end = System.currentTimeMillis();

        System.out.println("Time: "+(end-start)+"ms");
        return (int)(end-start);
    }

    private static String readFile(String path) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(path));

        StringBuilder sb = new StringBuilder();
        String line;

        while((line = in.readLine()) != null) {
            sb.append(line);
        }
        in.close();
        return sb.toString();
    }
}
