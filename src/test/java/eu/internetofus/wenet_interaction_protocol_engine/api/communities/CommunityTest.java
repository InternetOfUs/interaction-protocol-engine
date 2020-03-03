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

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import eu.internetofus.wenet_interaction_protocol_engine.ModelTestCase;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationsTest;

/**
 * Test the {@link Community}.
 *
 * @see Community
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunityTest extends ModelTestCase<Community> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Community createModelExample(int index) {

		final Community model = new Community();
		model._id = null;
		model.name = "Community " + index;
		model.description = "Description of community " + index;
		model.keywords = new ArrayList<>();
		for (int i = index - 2; i < index + 2; i++) {

			model.keywords.add("keyword " + i);
		}
		model.avatar = "http://images.com/avatar" + index + ".png";
		model.sinceTime = 1571412479710l + index * 300000;

		return model;

	}

	/**
	 * Check that an empty model is valid.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldEmptyModelBeValid() {

		final Community model = new Community();

		assertThat(catchThrowable(() -> model.validate("codePrefix"))).doesNotThrowAnyException();

	}

	/**
	 * Check that the {@link #createModelExample(int)} is valid.
	 *
	 * @param index to verify
	 *
	 * @see Community#validate(String)
	 */
	@ParameterizedTest(name = "The model example {0} has to be valid")
	@ValueSource(ints = { 0, 1, 2, 3, 4, 5 })
	public void shouldExampleBeValid(int index) {

		final Community model = this.createModelExample(index);
		assertThat(catchThrowable(() -> model.validate("codePrefix"))).doesNotThrowAnyException();

	}

	/**
	 * Check that the model with id is not valid.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnId() {

		final Community model = new Community();
		model._id = "has_id";
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix._id");

	}

	/**
	 * Check that the model with a large name.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnLargeName() {

		final Community model = new Community();
		model.name = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.name");

	}

	/**
	 * Check that the model with a large description.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnLargeDescription() {

		final Community model = new Community();
		model.description = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.description");

	}

	/**
	 * Check that the model with a large keyword.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnLargeKeyword() {

		final Community model = new Community();
		model.keywords = new ArrayList<>();
		model.keywords.add(ValidationsTest.STRING_256);
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.keywords[0]");

	}

	/**
	 * Check that the model with a bad avatar.
	 *
	 * @see Community#validate(String)
	 */
	@Test
	public void shouldNotBeValidWithAnBadAvatar() {

		final Community model = new Community();
		model.avatar = "image.png";
		assertThat(assertThrows(ValidationErrorException.class, () -> model.validate("codePrefix")).getCode())
				.isEqualTo("codePrefix.avatar");

	}

	/**
	 * Verify cannot merge with a large name.
	 *
	 * @see Community#merge(Community, String)
	 */
	@Test
	public void shouldNotMergeWithAnLargeName() {

		final Community target = new Community();
		final Community source = new Community();
		source.name = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.name");

	}

	/**
	 * Verify cannot merge with a large description.
	 *
	 * @see Community#merge(Community, String)
	 */
	@Test
	public void shouldNotMergeWithAnLargeDescription() {

		final Community target = new Community();
		final Community source = new Community();
		source.description = ValidationsTest.STRING_256;
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.description");

	}

	/**
	 * Verify cannot merge with a large keyword.
	 *
	 * @see Community#merge(Community, String)
	 */
	@Test
	public void shouldNotMergeWithAnLargeKeyword() {

		final Community target = new Community();
		final Community source = new Community();
		source.keywords = new ArrayList<>();
		source.keywords.add(ValidationsTest.STRING_256);
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.keywords[0]");

	}

	/**
	 * Verify cannot merge with a bad avatar.
	 *
	 * @see Community#merge(Community, String)
	 */
	@Test
	public void shouldNotMergeWithAnBadAvatar() {

		final Community target = new Community();
		final Community source = new Community();
		source.avatar = "avatar.png";
		assertThat(assertThrows(ValidationErrorException.class, () -> target.merge(source, "codePrefix")).getCode())
				.isEqualTo("codePrefix.avatar");

	}

	/**
	 * Verify can merge a {@code null} model.
	 *
	 * @see Community#merge(Community, String)
	 */
	@Test
	public void shouldMergeNullCommunity() {

		final Community target = new Community();
		assertThat(target.merge(null, "codePrefix")).isSameAs(target);

	}

	/**
	 * Check that can merge a {@link #createModelExample(int)}.
	 *
	 * @param index to the example to merge
	 *
	 * @see Community#validate(String)
	 */
	@ParameterizedTest(name = "The model example {0} has to be merged")
	@ValueSource(ints = { 0, 1, 2, 3, 4, 5 })
	public void shouldMergeExample(int index) {

		final Community target = new Community();
		target._id = "Identifier";
		target.sinceTime = 100 + index;
		final Community source = this.createModelExample(index);
		source._id = String.valueOf(index);
		source.sinceTime = index;
		final Community merged = target.merge(source, "codePrefix");
		assertThat(merged).isNotSameAs(source).isNotEqualTo(source);
		source._id = "Identifier";
		source.sinceTime = 100 + index;
		assertThat(merged).isEqualTo(source);
	}

}
