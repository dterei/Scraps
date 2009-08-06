/**
 * nullType
 *
 * Just testing ability to have a typed null.
 */
public class nullType {
	
	public static void main(String args[]) {
		nullParam((String) null);
		nullParam((Integer) null);
	}

	public static void nullParam(String a) {
		a = a == null ? "(null)" : a;
		System.out.println("String a is " + a);
	}

	public static void nullParam(Integer a) {
		String b = a == null ? "(null)"  : String.valueOf(a);
		System.out.println("Integer a is " + b);
	}

}
