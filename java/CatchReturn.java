/**
 * Test returning in a catch still runs the final.
 */
public class CatchReturn {

	public static void main(String[] args) {
		try {
			System.out.println("Hello World from try!");
			throw new Exception();
		} catch (Exception e) {
			return;
		} finally {
			System.out.println("Hello World from finally!");
		}
	}
}
