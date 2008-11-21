import java.util.*;

public class iter {

	public static void iterTestDelete(){
		List<String> trees = Arrays.asList("Maple", "Birch", "Poplar");
		Iterator<String> iter = trees.iterator();
		while (iter.hasNext()) {
			String n = iter.next();
			log(n);
			if (n == "Birch")
				iter.remove();
		}

		System.out.println("");

		while (iter.hasNext()) {
			log(iter.next());
		}
	}

	private static void log(String aMessage){
		System.out.println(aMessage);
	}

	public static void main(String[] args) {
		iterTestDelete();
	}

}

