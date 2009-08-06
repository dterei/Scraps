/**
 * regex
 *
 * Test java's version of regex.
 */
public class regex {
	
	public static void main(String[] args) {
		String in = "\"David < Terei\" <davidterei@gmail.com>";
		String reg = "\".*\"";

		System.out.println("In: " + in);
		System.out.println("Regex: " + reg);

		String out = in.replaceAll(reg, "");

		System.out.println("Out: " + out);
	}
}

