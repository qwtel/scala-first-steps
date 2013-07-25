import java.io.*;
import java.util.HashMap;

public class CSVTokenizerCL {
    public static void main(String[] args) throws IOException {
        if (args.length == 2 && args[0].equals("-t")) {
            BufferedReader in = new BufferedReader(new FileReader(args[1]));
            String text;

            CSVTokenizer n;
            while ((text = in.readLine()) != null) {
                n = new CSVTokenizer(text);
                while(n.hasMoreTokens()) {
                    System.out.println(n.nextToken());
                }
            }
            in.close();
        } else if (args.length == 2 && args[0].equals("-r")) {
            BufferedReader in = new BufferedReader(new FileReader(args[1]));
            String text;

            while((text = in.readLine()) != null) {
                text = replaceUmlauts(text);
                System.out.println(text);
            }
            in.close();
        } else {
            System.out.println("Tokenize: java CSVTokenizerCL -t test.csv");
            System.out.println("Replace: java CSVTokenizerCL -r test.txt");
        }
    }

    /*
     * Task 2
     */
    private static final HashMap<Character, String> escapeCodes;
    static {
        escapeCodes = new HashMap<Character, String>();
        escapeCodes.put('ä', "&Auml;");
        escapeCodes.put('Ä', "&auml;");
        escapeCodes.put('ö', "&Ouml;");
        escapeCodes.put('Ö', "&ouml;");
        escapeCodes.put('ü', "&Uuml;");
        escapeCodes.put('Ü', "&uuml;");
        escapeCodes.put('ß', "&szlig;");
    }

    private static StringBuilder sb1 = new StringBuilder();
    private static StringBuilder sb2 = new StringBuilder();

    public static String replaceUmlauts(String str) {

        // fixed size for performance
        // assuming the result will be approx 10% larger
        int approxCapacity = (int)(1.1 * str.length());
        sb1.ensureCapacity(approxCapacity);
        sb2.ensureCapacity(approxCapacity);

        sb1.delete(0, sb1.length());
        sb1.append(str);

        for(Character umlaut : escapeCodes.keySet()) {
            replace(umlaut, escapeCodes.get(umlaut));
        }

        return sb1.toString();
    }

    private static void replace(char search, String replacement) {
        sb2.delete(0, sb2.length());

        int startIndex = 0;
        int searchIndex = sb1.indexOf(String.valueOf(search));
        if (searchIndex == -1) {
            return;
        }

        while(searchIndex != -1) {
            String untilNextOccurrence = sb1.substring(startIndex, searchIndex);
            sb2.append(untilNextOccurrence).append(replacement);
            startIndex = searchIndex + 1;
            searchIndex = sb1.indexOf(String.valueOf(search), startIndex);
        }

        sb2.append(sb1.substring(startIndex));

        swap();
    }

    private static void swap() {
        StringBuilder temp;
        temp = sb1;
        sb1 = sb2;
        sb2 = temp;
    }
}
