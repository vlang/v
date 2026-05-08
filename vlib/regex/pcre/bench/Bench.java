import java.util.regex.*;
import java.util.ArrayList;
import java.util.List;

public class Bench {
    static final int ITERATIONS = 10000;
    static final int WARMUP = 100;
    
    static String shortText = "The quick brown fox jumps over the lazy dog 12345";
    static String mediumText = "The quick brown fox jumps over the lazy dog 12345. " +
        "Pack my box with five dozen liquor jugs. " +
        "How vexingly quick daft zebras jump! " +
        "The five boxing wizards jump quickly. " +
        "Sphinx of black quartz, judge my vow. " +
        "Two driven jocks help fax my big quiz.";
    static String longText = mediumText.repeat(20);
    static String emailText = "Contact us at user@example.com or admin@test.org for info. " +
        "Also try support@company.co.uk and test.user+tag@domain.com please.";
    
    interface BenchFunc { int run(); }
    
    static long[] bench(String name, BenchFunc f) {
        for (int i = 0; i < WARMUP; i++) f.run();
        long start = System.nanoTime();
        for (int i = 0; i < ITERATIONS; i++) f.run();
        long elapsed = System.nanoTime() - start;
        long nsPerOp = elapsed / ITERATIONS;
        return new long[]{nsPerOp};
    }
    
    public static void main(String[] args) {
        List<String[]> results = new ArrayList<>();
        
        // 1. Literal
        Pattern p1 = Pattern.compile("quick");
        long[] r1 = bench("1. Literal match", () -> p1.matcher(shortText).find() ? 1 : 0);
        results.add(new String[]{"1. Literal match (short)", String.valueOf(r1[0])});
        
        // 2. Char class
        Pattern p2 = Pattern.compile("[a-zA-Z]+");
        long[] r2 = bench("2", () -> p2.matcher(mediumText).find() ? 1 : 0);
        results.add(new String[]{"2. Char class [a-zA-Z]+", String.valueOf(r2[0])});
        
        // 3. Alternation
        Pattern p3 = Pattern.compile("fox|dog|cat|bird");
        long[] r3 = bench("3", () -> p3.matcher(shortText).find() ? 1 : 0);
        results.add(new String[]{"3. Alternation (4 words)", String.valueOf(r3[0])});
        
        // 4. Find all
        Pattern p4 = Pattern.compile("\\d+");
        long[] r4 = bench("4", () -> { int c=0; Matcher m=p4.matcher(mediumText); while(m.find()) c++; return c; });
        results.add(new String[]{"4. Find all digits", String.valueOf(r4[0])});
        
        // 5. Groups
        Pattern p5 = Pattern.compile("(\\w+)\\s+(\\w+)");
        long[] r5 = bench("5", () -> p5.matcher(mediumText).find() ? 1 : 0);
        results.add(new String[]{"5. Groups (\\w+)\\s+(\\w+)", String.valueOf(r5[0])});
        
        // 6. Email
        Pattern p6 = Pattern.compile("[\\w.]+@[\\w]+\\.[\\w]+");
        long[] r6 = bench("6", () -> { int c=0; Matcher m=p6.matcher(emailText); while(m.find()) c++; return c; });
        results.add(new String[]{"6. Email pattern", String.valueOf(r6[0])});
        
        // 7. Long text
        Pattern p7 = Pattern.compile("quickly");
        long[] r7 = bench("7", () -> { int c=0; Matcher m=p7.matcher(longText); while(m.find()) c++; return c; });
        results.add(new String[]{"7. Long text literal scan", String.valueOf(r7[0])});
        
        // 8. Replace
        Pattern p8 = Pattern.compile("\\d+");
        long[] r8 = bench("8", () -> p8.matcher(mediumText).replaceAll("NUM").length());
        results.add(new String[]{"8. Replace all digits", String.valueOf(r8[0])});
        
        // 9. Dot-star
        Pattern p9 = Pattern.compile(".*fox.*dog");
        long[] r9 = bench("9", () -> p9.matcher(shortText).matches() ? 1 : 0);
        results.add(new String[]{"9. Dot-star greedy", String.valueOf(r9[0])});
        
        // 10. Compile
        long[] r10 = bench("10", () -> { Pattern.compile("\\w+@\\w+\\.\\w+"); return 1; });
        results.add(new String[]{"10. Compile simple", String.valueOf(r10[0])});
        
        // 11. Lookahead
        Pattern p11 = Pattern.compile("\\w+(?=\\.)");
        long[] r11 = bench("11", () -> p11.matcher(emailText).find() ? 1 : 0);
        results.add(new String[]{"11. Lookahead \\w+(?=\\.)", String.valueOf(r11[0])});
        
        // 12. Backref
        String doubled = "the the quick quick fox fox jumps over the lazy dog";
        Pattern p12 = Pattern.compile("(\\w+)\\s+\\1");
        long[] r12 = bench("12", () -> { int c=0; Matcher m=p12.matcher(doubled); while(m.find()) c++; return c; });
        results.add(new String[]{"12. Backref (\\w+)\\s+\\1", String.valueOf(r12[0])});
        
        System.out.println("=".repeat(70));
        System.out.println("                 JAVA REGEX BENCHMARK");
        System.out.println("=".repeat(70));
        System.out.printf("%-35s %12s %14s%n", "Benchmark", "ns/op", "ops/sec");
        System.out.println("-".repeat(70));
        for (String[] r : results) {
            long ns = Long.parseLong(r[1]);
            double ops = ns > 0 ? 1e9 / ns : 0;
            System.out.printf("%-35s %12d %14.0f%n", r[0], ns, ops);
        }
        System.out.println("=".repeat(70));
    }
}
