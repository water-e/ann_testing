package wrims;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class FortranReshapeParserTest
{

	@Test
	void testParseReshapeSimple()
	{
		String fortranCode =
				"Reshape((/ 1.0, 2.0, 3.0, 4.0 /), (/ 2, 2 /))";
		FortranReshapeParser.ReshapeResult result = FortranReshapeParser.parseReshapeSnippet(fortranCode);
		assertNotNull(result);
		assertArrayEquals(new double[] {1.0, 2.0, 3.0, 4.0}, result.getValues(), 0.0001);
		assertArrayEquals(new int[] {2, 2}, result.getDimensions());
	}

	@Test
	void testParseReshapeBigger()
	{
		String fortranCode =
				"Reshape((/-0.54676,-7.737 &\n        ,-1.6776,-1.7751,0.15857,-25.3483 &\n        ,0.17588,15.0242 &\n        ,-1.124,-8.7763 &\n        ,-1.9823,9.7069 &\n        ,2.072,5.3226 &\n        ,2.291,10.9932 &\n        /),(/2,8/))";

		FortranReshapeParser.ReshapeResult result = FortranReshapeParser.parseReshapeSnippet(fortranCode);
		assertNotNull(result);

		double[] values = result.getValues();
		assertEquals(16, values.length);
		assertEquals(-0.54676, values[0], 0.00001);
		assertEquals(-7.737, values[1], 0.00001);
		assertEquals(-1.6776, values[2], 0.00001);

		assertEquals(10.9932, values[15], 0.00001);

		assertArrayEquals(new int[] {2, 8}, result.getDimensions());
	}

	@Test
	void testParseFortranDoubleArray(){
		String toParse = "(/0.73999,-2.3748,-0.84983,-2.148,-4.475,-6.3818,-2.3283,-0.40998/)";
		double[] result = FortranReshapeParser.parseFortranDoubleArray(toParse);

		assertNotNull(result);
		assertEquals(8, result.length);
		assertEquals(0.73999, result[0], 0.00001);
		assertEquals(-2.3748, result[1], 0.00001);
		assertEquals(-0.84983, result[2], 0.00001);
		assertEquals(-2.148, result[3], 0.00001);
		assertEquals(-4.475, result[4], 0.00001);
		assertEquals(-6.3818, result[5], 0.00001);
		assertEquals(-2.3283, result[6], 0.00001);
		assertEquals(-0.40998, result[7], 0.00001);

	}

}