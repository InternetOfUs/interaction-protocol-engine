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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import eu.internetofus.common.api.models.ModelTestCase;
import eu.internetofus.common.api.models.ValidationErrorException;
import eu.internetofus.common.api.models.ValidationsTest;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.Norm;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.NormOperator;

/**
 * Test the {@link Norm}
 *
 * @see Norm
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormTest extends ModelTestCase<Norm> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Norm createModelExample(int index) {

		final Norm norm = new Norm();
		norm.id = null;
		norm.attribute = "attribute_" + index;
		norm.operator = NormOperator.EQUALS;
		norm.comparison = "comparison_" + index;
		norm.negation = true;
		return norm;
	}

	/**
	 * Check that the {@link #createModelExample(int)} is valid.
	 *
	 * @see Norm#validate(String)
	 */
	@Test
	public void shouldExample1BeValid() {

		final Norm model = this.createModelExample(1);
		assertThat(catchThrowable(() -> model.validate("codePrefix"))).doesNotThrowAnyException();
	}

	/**
	 * Check that a model with all the values is valid.
	 *
	 * @see Norm#validate(String)
	 */
	@Test
	public void shouldFullModelBeValid() {

		final Norm model = new Norm();
		model.id = "      ";
		model.attribute = "    attribute    ";
		model.operator = NormOperator.GREATER_THAN;
		model.comparison = "   comparison    ";
		model.negation = false;
		assertThat(catchThrowable(() -> model.validate("codePrefix"))).doesNotThrowAnyException();

		final Norm expected = new Norm();
		expected.id = model.id;
		expected.attribute = "attribute";
		expected.operator = NormOperator.GREATER_THAN;
		expected.comparison = "comparison";
		expected.negation = false;
		assertThat(model).isEqualTo(expected);
	}

	/**
	 * Check that the model with id is not valid.
	 *
	 * @see Norm#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnId() {

		final Norm model = new Norm();
		model.id = "has_id";
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.id");
	}

	/**
	 * Check that not accept norms with bad attribute.
	 *
	 * @see Norm#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithABadAttribute() {

		final Norm model = new Norm();
		model.attribute = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.attribute");
	}

	/**
	 * Check that not accept norms with bad comparison.
	 *
	 * @see Norm#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithABadComparison() {

		final Norm model = new Norm();
		model.comparison = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.comparison");
	}

	/**
	 * Check that not merge with bad attribute.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldNotMergeWithABadAttribute() {

		final Norm target = new Norm();
		final Norm source = new Norm();
		source.attribute = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.attribute");
	}

	/**
	 * Check that not merge with bad comparison.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldNotMergeWithABadComparison() {

		final Norm target = new Norm();
		final Norm source = new Norm();
		source.comparison = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.comparison");
	}

	/**
	 * Check that merge.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMerge() {

		final Norm target = this.createModelExample(1);
		final Norm source = this.createModelExample(2);
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotEqualTo(target).isEqualTo(source);
	}

	/**
	 * Check that merge with {@code null}.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeWithNull() {

		final Norm target = this.createModelExample(1);
		final Norm merged = target.merge(null, "codePrefix");
		assertThat(merged).isSameAs(target);
	}

	/**
	 * Check that merge only the identifier.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeOnlyId() {

		final Norm target = this.createModelExample(1);
		target.id = "1";
		final Norm source = new Norm();
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isEqualTo(target).isNotSameAs(target).isNotEqualTo(source);
	}

	/**
	 * Check that merge only the attribute.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeOnlyAttribute() {

		final Norm target = this.createModelExample(1);
		final Norm source = new Norm();
		source.attribute = "NEW VALUE";
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotEqualTo(target).isNotEqualTo(source);
		target.attribute = "NEW VALUE";
		assertThat(merged).isEqualTo(target);
	}

	/**
	 * Check that merge only the operator.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeOnlyOperator() {

		final Norm target = this.createModelExample(1);
		final Norm source = new Norm();
		source.operator = NormOperator.GREATER_THAN;
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotEqualTo(target).isNotEqualTo(source);
		target.operator = NormOperator.GREATER_THAN;
		assertThat(merged).isEqualTo(target);
	}

	/**
	 * Check that merge only the comparison.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeOnlyComparison() {

		final Norm target = this.createModelExample(1);
		final Norm source = new Norm();
		source.comparison = "NEW VALUE";
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotEqualTo(target).isNotEqualTo(source);
		target.comparison = "NEW VALUE";
		assertThat(merged).isEqualTo(target);
	}

	/**
	 * Check that merge only the negation.
	 *
	 * @see Norm#merge(Norm, String)
	 */
	@Test
	public void shouldMergeOnlyNegation() {

		final Norm target = this.createModelExample(1);
		final Norm source = new Norm();
		source.negation = false;
		final Norm merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotEqualTo(target).isNotEqualTo(source);
		target.negation = false;
		assertThat(merged).isEqualTo(target);
	}

}
