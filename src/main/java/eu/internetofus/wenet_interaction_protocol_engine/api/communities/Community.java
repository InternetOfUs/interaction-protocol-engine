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

import java.util.List;

import eu.internetofus.wenet_interaction_protocol_engine.Mergeable;
import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.Validable;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.Validations;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A community that an user plays.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(name = "Community", description = "This component describes a community of users.")
public class Community extends Model implements Validable, Mergeable<Community> {

	/**
	 * The identifier of the community.
	 */
	@Schema(description = "The identifier of the community.", example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String _id;

	/**
	 * The name of the community.
	 */
	@Schema(description = "The name of the community.", example = "WeNet")
	public String name;

	/**
	 * The description of the community.
	 */
	@Schema(
			description = "The description of the community.",
			example = "A community of users that provide or require help")
	public String description;

	/**
	 * The description of the community.
	 */
	@ArraySchema(
			schema = @Schema(implementation = String.class),
			arraySchema = @Schema(
					description = "The keywords of the community",
					example = "[\"social interaction\",\"ethics\",\"diversity\"]"))
	public List<String> keywords;

	/**
	 * The avatar of the community.
	 */
	@Schema(
			description = "The URL to an image that represents the avatar of the community.",
			example = "https://internetofus.eu/wp-content/uploads/sites/38/2019/02/WeNet_logo.png")
	public String avatar;

	/**
	 * The time in ISO 8601 format since the community is active.
	 */
	@Schema(
			description = "The difference, measured in milliseconds, between the community is active and midnight, January 1, 1970 UTC.",
			example = "1571412479710")
	public long sinceTime;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void validate(String codePrefix) throws ValidationErrorException {

		this._id = Validations.validateNullableStringField(codePrefix, "id", 255, this._id);
		if (this._id != null) {

			throw new ValidationErrorException(codePrefix + "._id",
					"You can not specify the identifier of the community to create");

		}
		this.name = Validations.validateNullableStringField(codePrefix, "name", 255, this.name);
		this.description = Validations.validateNullableStringField(codePrefix, "description", 255, this.description);
		this.keywords = Validations.validateNullableListStringField(codePrefix, "keywords", 255, this.keywords);
		this.avatar = Validations.validateNullableURLField(codePrefix, "avatar", this.avatar);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Community merge(Community source, String codePrefix) throws ValidationErrorException {

		if (source == null) {

			return this;

		} else {

			final Community merged = new Community();
			merged.name = source.name;
			if (merged.name == null) {

				merged.name = this.name;
			}

			merged.description = source.description;
			if (merged.description == null) {

				merged.description = this.description;
			}

			merged.keywords = source.keywords;
			if (merged.keywords == null) {

				merged.keywords = this.keywords;
			}

			merged.avatar = source.avatar;
			if (merged.avatar == null) {

				merged.avatar = this.avatar;
			}

			merged.validate(codePrefix);

			merged._id = this._id;
			merged.sinceTime = this.sinceTime;
			return merged;
		}
	}
}
