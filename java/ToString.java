class ToString {

	public static void main(String args[]) {
		System.out.println("Hello World");

		char c = 'a';
		System.out.println("Printing char...");
		System.out.println(c);

		int i = 9;
		System.out.println("Printing int...");
		System.out.println(i);

		System.out.println("Printing char + int...");
		System.out.println(c + i); // wrong! adds them as numbers

		System.out.println("Printing \"\" + char + int...");
		System.out.println("" + c + i);

		System.out.println("Printing char + Integer.toString(int)...");
		System.out.println(c + Integer.toString(i));

		System.out.println("Printing Character.toString(char) + int...");
		System.out.println(Character.toString(c) + i);

		System.out.println("Printing String(char) + int...");
		char[] chars = {c};
		System.out.println(new String(chars) + i);
	}

}

