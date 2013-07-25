import java.util.ArrayList;
import java.util.Enumeration;
import java.util.NoSuchElementException;

public class CSVTokenizer implements Enumeration<Object> {

    private static char ESCAPE = '"';
    private static char DELIMITER = ';';
    private ArrayList<String> tokens = new ArrayList<String>();
    private int i = 0;

    public CSVTokenizer(String str) {
        boolean insideEscape = false;
        StringBuilder word = new StringBuilder();

        for(int j = 0; j < str.length(); j++) {
            char c = str.charAt(j);

            if (insideEscape) {
                if (c == ESCAPE) {
                    if (j+1 < str.length() && str.charAt(j+1) == ESCAPE) {
                        word.append(ESCAPE);
                        j++;
                    } else {
                        insideEscape = false;
                    }
                } else {
                    word.append(c);
                }
            } else {
                if (c == ESCAPE) {
                    insideEscape = true;
                } else if (c == DELIMITER) {
                    tokens.add(word.toString());
                    word = new StringBuilder();
                } else {
                    word.append(c);
                }
            }
        }

        tokens.add(word.toString());
    }

    public boolean hasMoreTokens() {
        return (countTokens() > 0);
    }

    public String nextToken() {
        if (hasMoreTokens()) {
            return tokens.get(i++);
        } else {
            throw new NoSuchElementException();
        }
    }

    public int countTokens() {
        return tokens.size() - i;
    }

    @Override
    public boolean hasMoreElements() {
        return this.hasMoreTokens();
    }

    @Override
    public Object nextElement() {
        return this.nextToken();
    }
}
