public class mat {

	public static void main(String[] args) {
		int a = 20;
		int b = 10;

		double z = Math.ceil(((float) a) % b);

		System.out.printf("z = %f\n", z);

		a = 35;
		float x = a % b;
		System.out.printf("%d %% %d = %f\n", a, b, x);
	}

}

