package wrims;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FortranReshapeParser
{
	public static class ReshapeResult
	{
		private final double[] values;
		private final int[] dimensions;

		public ReshapeResult(double[] arrayValues, int[] dimensions)
		{
			this.values = arrayValues;
			this.dimensions = dimensions;
		}

		public double[] getValues()
		{
			return values;
		}

		public int[] getDimensions()
		{
			return dimensions;
		}
	}

	public static ReshapeResult parseReshapeSnippet(String fortranCode)
	{
		Pattern reshapePattern = Pattern.compile("Reshape\\(" + // startsWith Reshape\(
						"(\\(\\s*.*?\\s*\\))" +  // group 1
						"\\s*,\\s*" +            // sep by comma with or without whitespace
						"(\\(\\s*.*?\\s*\\))" +  // group 2
						"\\)",   // endsWith \)
				Pattern.DOTALL);  // DOTALL allows . to match \n

		Matcher reshapeMatcher = reshapePattern.matcher(fortranCode);
		if(reshapeMatcher.find())
		{
			String arrayValuesStr = reshapeMatcher.group(1);
			String dimensionsStr = reshapeMatcher.group(2);

			double[] arrayValues = parseFortranDoubleArray(arrayValuesStr);
			int[] dimensions = parseFortranIntArray(dimensionsStr);

			return new ReshapeResult(arrayValues, dimensions);
		}
		else
		{
			return null;
		}
	}


	public static double[] parseFortranDoubleArray(String arrayString)
	{
		String trimmed = arrayString.trim();
		if(trimmed.startsWith("(/") && trimmed.endsWith("/)"))
		{
			String valuesStr = trimmed.substring(2, trimmed.length() - 2);
			String[] valueStrings = valuesStr.split(",");

			double[] arrayValues = new double[valueStrings.length];
			for(int i = 0; i < valueStrings.length; i++)
			{
				String valueStr = valueStrings[i].trim();
				if(valueStr.endsWith(" &"))
				{
					valueStr = valueStr.substring(0, valueStr.length() - 1);
				}
				arrayValues[i] = Double.parseDouble(valueStr);
			}

			return arrayValues;
		}
		else
		{
			throw new IllegalArgumentException("Invalid Fortran array format");
		}
	}

	public static int[] parseFortranIntArray(String arrayString)
	{
		String trimmed = arrayString.trim();
		if(trimmed.startsWith("(/") && trimmed.endsWith("/)"))
		{
			String valuesStr = trimmed.substring(2, trimmed.length() - 2);
			String[] valueStrings = valuesStr.split(",");

			int[] arrayValues = new int[valueStrings.length];
			for(int i = 0; i < valueStrings.length; i++)
			{
				arrayValues[i] = Integer.parseInt(valueStrings[i].trim());
			}

			return arrayValues;
		}
		else
		{
			throw new IllegalArgumentException("Invalid Fortran array format");
		}
	}

	public static void main(String[] args)
	{
		String fortranCode = "Reshape((/-0.54676,-7.737 &\n        ,-1.6776,-1.7751,0.15857,-25.3483 &\n        ,0.17588,15.0242 &\n        ,-1.124,-8.7763 &\n        ,-1.9823,9.7069 &\n        ,2.072,5.3226 &\n        ,2.291,10.9932 &\n        /),(/2,8/))";

		ReshapeResult result = parseReshapeSnippet(fortranCode);
		if(result != null)
		{
			System.out.println("Array Values:");
			for(double value : result.getValues())
			{
				System.out.print(value + " ");
			}
			System.out.println("\nDimensions: " + Arrays.toString(result.getDimensions()));
		}
		else
		{
			System.out.println("No Reshape call found.");
		}
	}
}
