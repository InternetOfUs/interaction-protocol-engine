/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 * Test the {@link Validations}.
 *
 * @see Validations
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class ValidationsTest {

	/**
	 * A string with 256 characters.
	 */
	public static final String STRING_256 = "0Ncu2eQI7boSct2Ga6VHViEPJn0HqffPajWKyL3TmgUyLG4ZjVLbaZSx7DZXuY0EAWGqWnWOB35Uql92cV2zTbBrSi4gVR0y9jJ3a5zsHnnXNFucmHRyplXw0v98l7BD4d8jvKro7QBIuZM4A4fARUol9gSrRAIoZ7PpUxtbNfteFkVhxfRUhGAkHKfRUsMulmgui5bRQaCM8ivevTJm8N4jXXUlgfkPepeMsQeQPzktJRnZDR3PxDrKLtKjoE24";

	/**
	 * Check that a field can be null.
	 *
	 * @see Validations#validateNullableStringField(String, String, int,String)
	 */
	@Test
	public void shouldNullStringFieldBeValid() {

		assertThat(Validations.validateNullableStringField("codePrefix", "fieldName", 255, null)).isEqualTo(null);
	}

	/**
	 * Check that an empty is right but is changed to null.
	 *
	 * @see Validations#validateNullableStringField(String, String, int,String)
	 */
	@Test
	public void shouldEmptyStringFieldBeValid() {

		assertThat(Validations.validateNullableStringField("codePrefix", "fieldName", 255, "")).isEqualTo(null);
	}

	/**
	 * Check that an white value is right but is changed to null.
	 *
	 * @see Validations#validateNullableStringField(String, String, int,String)
	 */
	@Test
	public void shouldWhiteStringFieldBeValid() {

		assertThat(Validations.validateNullableStringField("codePrefix", "fieldName", 255, "       ")).isEqualTo(null);
	}

	/**
	 * Check that the value is trimmed to be valid.
	 *
	 * @see Validations#validateNullableStringField(String, String, int,String)
	 */
	@Test
	public void shouldStringWithWhiteFieldBeValid() {

		assertThat(Validations.validateNullableStringField("codePrefix", "fieldName", 255, "   a b c    "))
				.isEqualTo("a b c");
	}

	/**
	 * Check that the value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableStringField(String, String, int,String)
	 */
	@Test
	public void shouldNotBeValidIfStringIsTooLarge() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableStringField("codePrefix", "fieldName", 255, ValidationsTest.STRING_256))
						.getCode()).isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that an email field can be null.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldNullEmailFieldBeValid() {

		assertThat(Validations.validateNullableEmailField("codePrefix", "fieldName", null)).isEqualTo(null);
	}

	/**
	 * Check that an empty email is right but is changed to null.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldEmptyEmailFieldBeValid() {

		assertThat(Validations.validateNullableEmailField("codePrefix", "fieldName", "")).isEqualTo(null);
	}

	/**
	 * Check that an white email value is right but is changed to null.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldWhiteEmailFieldBeValid() {

		assertThat(Validations.validateNullableEmailField("codePrefix", "fieldName", "       ")).isEqualTo(null);
	}

	/**
	 * Check that the email value is trimmed to be valid.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldEmailWithWhiteFieldBeValid() {

		assertThat(Validations.validateNullableEmailField("codePrefix", "fieldName", "   a@b.com    "))
				.isEqualTo("a@b.com");
	}

	/**
	 * Check that the email value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidIfEmailIsTooLarge() {

		assertThat(assertThrows(ValidationErrorException.class, () -> Validations.validateNullableEmailField("codePrefix",
				"fieldName", ValidationsTest.STRING_256.substring(0, 250) + "@b.com")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that the email value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableEmailField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidABadEmailValue() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableEmailField("codePrefix", "fieldName", "bad email(at)host.com")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that a field can be null.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldNullLocaleFieldBeValid() {

		assertThat(Validations.validateNullableLocaleField("codePrefix", "fieldName", null)).isEqualTo(null);
	}

	/**
	 * Check that an empty is right but is changed to null.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldEmptyLocaleFieldBeValid() {

		assertThat(Validations.validateNullableLocaleField("codePrefix", "fieldName", "")).isEqualTo(null);
	}

	/**
	 * Check that an white value is right but is changed to null.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldWhiteLocaleFieldBeValid() {

		assertThat(Validations.validateNullableLocaleField("codePrefix", "fieldName", "       ")).isEqualTo(null);
	}

	/**
	 * Check that the locale value is trimmed to be valid.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldLocaleWithWhiteFieldBeValid() {

		assertThat(Validations.validateNullableLocaleField("codePrefix", "fieldName", "   en_US    ")).isEqualTo("en_US");
	}

	/**
	 * Check that the locale value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidIfLocaleIsTooLarge() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableLocaleField("codePrefix", "fieldName", "to_la_ge_")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that the locale value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableLocaleField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidABadLocaleValue() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableLocaleField("codePrefix", "fieldName", "de-Gr")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that an empty is right but is changed to null.
	 *
	 * @see Validations#validateNullableDateField(String, String,
	 *      DateTimeFormatter,String)
	 */
	@Test
	public void shouldEmptyDateFieldBeValid() {

		assertThat(Validations.validateNullableDateField("codePrefix", "fieldName", DateTimeFormatter.ISO_INSTANT, ""))
				.isEqualTo(null);
	}

	/**
	 * Check that an white value is right but is changed to null.
	 *
	 * @see Validations#validateNullableDateField(String, String,
	 *      DateTimeFormatter,String)
	 */
	@Test
	public void shouldWhiteDateFieldBeValid() {

		assertThat(
				Validations.validateNullableDateField("codePrefix", "fieldName", DateTimeFormatter.ISO_INSTANT, "       "))
						.isEqualTo(null);
	}

	/**
	 * Check that the date value is trimmed to be valid.
	 *
	 * @see Validations#validateNullableDateField(String, String,
	 *      DateTimeFormatter,String)
	 */
	@Test
	public void shouldDateWithWhiteFieldBeValid() {

		assertThat(Validations.validateNullableDateField("codePrefix", "fieldName", DateTimeFormatter.ISO_INSTANT,
				"   2011-12-03t10:15:30z    ")).isEqualTo("2011-12-03T10:15:30Z");
	}

	/**
	 * Check that the date value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableDateField(String, String,
	 *      DateTimeFormatter,String)
	 */
	@Test
	public void shouldNotBeValidABadDateValue() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableDateField("codePrefix", "fieldName", null, "bad date")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that the date value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableDateField(String, String,
	 *      DateTimeFormatter,String)
	 */
	@Test
	public void shouldNotBeValidABadIsoinstanceValue() {

		assertThat(assertThrows(ValidationErrorException.class, () -> Validations.validateNullableDateField("codePrefix",
				"fieldName", DateTimeFormatter.ISO_INSTANT, "bad date")).getCode()).isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that an URL field can be null.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldNullURLFieldBeValid() {

		assertThat(Validations.validateNullableURLField("codePrefix", "fieldName", null)).isEqualTo(null);
	}

	/**
	 * Check that an empty URL is right but is changed to null.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldEmptyURLFieldBeValid() {

		assertThat(Validations.validateNullableURLField("codePrefix", "fieldName", "")).isEqualTo(null);
	}

	/**
	 * Check that an white URL value is right but is changed to null.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldWhiteURLFieldBeValid() {

		assertThat(Validations.validateNullableURLField("codePrefix", "fieldName", "       ")).isEqualTo(null);
	}

	/**
	 * Check that the URL value is trimmed to be valid.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldURLWithWhiteFieldBeValid() {

		assertThat(Validations.validateNullableURLField("codePrefix", "fieldName", "   http://host.com    "))
				.isEqualTo("http://host.com");
	}

	/**
	 * Check that the URL value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidIfURLIsTooLarge() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableURLField("codePrefix", "fieldName", "http://" + ValidationsTest.STRING_256))
						.getCode()).isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that the URL value of the field is not valid if it is too large.
	 *
	 * @see Validations#validateNullableURLField(String, String, String)
	 */
	@Test
	public void shouldNotBeValidABadURLValue() {

		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableURLField("codePrefix", "fieldName", "bad URL(at)host.com")).getCode())
						.isEqualTo("codePrefix.fieldName");
	}

	/**
	 * Check that a {@code null} string list is valid.
	 *
	 * @see Validations#validateNullableListStringField(String, String, int, List)
	 */
	@Test
	public void shouldNullStringListBeValid() {

		assertThat(Validations.validateNullableListStringField("codePrefix", "fieldName", 255, null)).isEqualTo(null);
	}

	/**
	 * Check that an empty string list is valid.
	 *
	 * @see Validations#validateNullableListStringField(String, String, int, List)
	 */
	@Test
	public void shouldEmptyStringListBeValid() {

		final List<String> values = new ArrayList<>();
		assertThat(Validations.validateNullableListStringField("codePrefix", "fieldName", 255, values)).isSameAs(values);

	}

	/**
	 * Check that a string list is valid.
	 *
	 * @see Validations#validateNullableListStringField(String, String, int, List)
	 */
	@Test
	public void shouldStringListBeValid() {

		final List<String> values = new ArrayList<>();
		values.add(null);
		values.add("       ");
		values.add("   value 34   ");
		values.add("value");
		assertThat(Validations.validateNullableListStringField("codePrefix", "fieldName", 255, values)).hasSize(2)
				.contains("value 34", "value");

	}

	/**
	 * Check that a string list is valid.
	 *
	 * @see Validations#validateNullableListStringField(String, String, int, List)
	 */
	@Test
	public void shouldStringListIsnotValid() {

		final List<String> values = new ArrayList<>();
		values.add(null);
		values.add("       ");
		values.add("   value 34   ");
		values.add("value");
		assertThat(assertThrows(ValidationErrorException.class,
				() -> Validations.validateNullableListStringField("codePrefix", "fieldName", 4, values)).getCode())
						.isEqualTo("codePrefix.fieldName[2]");

	}

}
