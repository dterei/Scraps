import java.io.*;
import java.util.regex.*;

public class Bench {

	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String s;
		while ((s = in.readLine()) != null && s.length() != 0) {
			validateQuery(s);
			//boolean b = validateQuery(s);
			//String c = cleanLine(s);
			//System.out.printf("%s -> %s [%b]\n", s, c, b);
		}
	}

	private static Pattern patternQString =
		Pattern.compile("\\band\\b|\\bor\\b|\\bnot\\b|\\p{Punct}", Pattern.CASE_INSENSITIVE);

	public static boolean validateQuery(String line) {
		if (line == null)
			return false;
		Matcher m = patternQString.matcher(line);
		if (m.replaceAll("").trim().length() == 0)
			return false;
		return true;
	}

	public static String cleanLine(String line) {
		if (line == null)
			return "";
		Matcher m = patternQString.matcher(line);
		return m.replaceAll("").trim();
	}

}

