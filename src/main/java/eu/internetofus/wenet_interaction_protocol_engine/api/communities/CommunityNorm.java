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

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.Validable;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.Validations;
import eu.internetofus.wenet_interaction_protocol_engine.api.norms.Norm;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A norm associated to a community.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "A norm that is applied in a community")
public class CommunityNorm extends Model implements Validable {

	/**
	 * The identifier of the community norm.
	 */
	@Schema(description = "The identifier of the community norm.", example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String _id;

	/**
	 * The difference, measured in milliseconds, between the norm is applied in the
	 * community and midnight, January 1, 1970 UTC.
	 */
	@Schema(
			description = "The difference, measured in seconds, between the norm is applied in the community and midnight, January 1, 1970 UTC.",
			example = "15678423")
	public long sinceTime;

	/**
	 * The norm to apply to the community.
	 */
	@Schema(
			description = "The norm to apply into the community.",
			ref = "https://bitbucket.org/wenet/wenet-components-documentation/raw/5c0512480f89ae267d6fc0dcf42db0f3a50d01e8/sources/wenet-models.yaml#/components/schemas/Norm")
	public Norm norm;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void validate(String codePrefix) throws ValidationErrorException {

		this._id = Validations.validateNullableStringField(codePrefix, "_id", 255, this._id);
		if (this._id != null) {

			throw new ValidationErrorException(codePrefix + "._id",
					"You can not specify the identifier of the community norm to create");

		}

		if (this.norm == null) {

			throw new ValidationErrorException(codePrefix + ".norm", "You must specify a norm");

		} else {

			this.norm.validate(codePrefix + ".norm");
		}

	}

}
