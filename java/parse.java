import static java.lang.System.out;

public class parse {

	public static void main(String[] args) {
		String test = "Hello World! How are you?";

		String[] a = test.split(" ");
		String[] b = test.split(";");

		out.printf("String: %s\n", test);

		out.printf("\nsplit( ) : ");
		printArray(a);
		out.printf(";\n");

		out.printf("\nsplit(;): ");
		printArray(b);
		out.printf(";\n");
	}

	private static void printArray(String[] s) {
		if (s.length <= 0) {
			return;
		}

		for (int i = 0; i < s.length - 1; i++) {
			out.printf("%s, ", s[i]);
		}

		out.printf("%s", s[s.length - 1]);
	}

}

