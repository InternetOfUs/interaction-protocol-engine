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

import java.util.UUID;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.Validable;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.Validations;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * The definition of a norm.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "A norm that has to be satisfied.")
public class Norm extends Model implements Validable {

	/**
	 * The identifier of the norm.
	 */
	@Schema(description = "The identifier of the norm.", example = "ceb84643-645a-4a55-9aaf-158370289eba")
	public String id;

	/**
	 * The name of the attribute whose value the norm should be compared to.
	 */
	@Schema(description = "The name of the attribute whose value the norm should	 be compared to.", example = "has_car")
	public String attribute;

	/**
	 * The operator of the norm.
	 */
	@Schema(description = "The operator of the norm.", example = "EQUALS")
	public NormOperator operator;

	/**
	 * The norm value for the comparison.
	 */
	@Schema(description = "The norm value for the comparison.", example = "true")
	public String comparison;

	/**
	 * Specified if a negation operator should be applied.
	 */
	@Schema(description = "The operator of the norm.", example = "true", defaultValue = "true")
	public boolean negation;

	/**
	 * Create a new norm.
	 */
	public Norm() {

		this.negation = true;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void validate(String codePrefix) throws ValidationErrorException {

		this.id = Validations.validateNullableStringField(codePrefix, "id", 255, this.id);
		if (this.id != null) {

			throw new ValidationErrorException(codePrefix + ".id",
					"You can not specify the identifier of the norm to create");

		} else {

			this.id = UUID.randomUUID().toString();
		}
		this.attribute = Validations.validateNullableStringField(codePrefix, "attribute", 255, this.attribute);
		this.comparison = Validations.validateNullableStringField(codePrefix, "comparison", 255, this.comparison);

	}

	/**
	 * Merge this model with another.
	 *
	 * @param source     to merge.
	 * @param codePrefix the prefix of the code to use for the error message.
	 *
	 * @return the merged model.
	 *
	 * @throws ValidationErrorException if the model is not right.
	 */
	public Norm merge(Norm source, String codePrefix) throws ValidationErrorException {

		if (source != null) {

			final Norm merged = new Norm();
			merged.attribute = source.attribute;
			if (merged.attribute == null) {

				merged.attribute = this.attribute;
			}

			merged.operator = source.operator;
			if (merged.operator == null) {

				merged.operator = this.operator;
			}

			merged.comparison = source.comparison;
			if (merged.comparison == null) {

				merged.comparison = this.comparison;
			}

			merged.negation = source.negation;

			merged.validate(codePrefix);
			merged.id = this.id;
			return merged;

		} else {
			return this;

		}
	}
}
