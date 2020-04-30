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

import eu.internetofus.common.api.models.ModelTestCase;
import eu.internetofus.common.api.models.wenet.NormTest;

/**
 * Test the {@link CommunityNorm}.
 *
 * @see CommunityNorm
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunityNormTest extends ModelTestCase<CommunityNorm> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public CommunityNorm createModelExample(int index) {

		final CommunityNorm model = new CommunityNorm();
		model._creationTs = index;
		model.norm = new NormTest().createModelExample(index);
		return model;

	}

	// /**
	// * Check that the {@link #createModelExample(int)} is valid.
	// *
	// * @see Norm#validate(String)
	// */
	// @Test
	// public void shouldExample1BeValid() {
	//
	// final CommunityNorm model = this.createModelExample(1);
	// assertThat(catchThrowable(() ->
	// model.validate("codePrefix"))).doesNotThrowAnyException();
	//
	// }
	//
	// /**
	// * Check that the model with id is not valid.
	// *
	// * @see CommunityNorm#validate(String)
	// */
	// @Test
	// public void shouldNotBeValidWithAnId() {
	//
	// final CommunityNorm model = new CommunityNorm();
	// model._id = "has_id";
	// assertThat(assertThrows(ValidationErrorException.class, () ->
	// model.validate("codePrefix")).getCode())
	// .isEqualTo("codePrefix._id");
	// }
	//
	// /**
	// * Check that the model without a norm is not valid.
	// *
	// * @see CommunityNorm#validate(String)
	// */
	// @Test
	// public void shouldNotBeValidWithoutNorm() {
	//
	// final CommunityNorm model = new CommunityNorm();
	// assertThat(assertThrows(ValidationErrorException.class, () ->
	// model.validate("codePrefix")).getCode())
	// .isEqualTo("codePrefix.norm");
	// }
	//
	// /**
	// * Check that the model with a bad norm is not be valid.
	// *
	// * @see CommunityNorm#validate(String)
	// */
	// @Test
	// public void shouldNotBeValidWithBadNorm() {
	//
	// final CommunityNorm model = new CommunityNorm();
	// model.norm = new Norm();
	// model.norm.attribute = ValidationsTest.STRING_256;
	// assertThat(assertThrows(ValidationErrorException.class, () ->
	// model.validate("codePrefix")).getCode())
	// .isEqualTo("codePrefix.norm.attribute");
	//
	// }

}
