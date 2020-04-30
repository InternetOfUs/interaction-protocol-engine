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

import eu.internetofus.common.api.models.Mergeable;
import eu.internetofus.common.api.models.Merges;
import eu.internetofus.common.api.models.Validable;
import eu.internetofus.common.api.models.ValidationErrorException;
import eu.internetofus.common.api.models.Validations;
import eu.internetofus.common.api.models.wenet.CreateUpdateTsDetails;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;

/**
 * A community that an user plays.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(name = "Community", description = "This component describes a community of users.")
public class Community extends CreateUpdateTsDetails implements Validable, Mergeable<Community> {

	/**
	 * The identifier of the community.
	 */
	@Schema(description = "The identifier of the community.", example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String id;

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
	 * {@inheritDoc}
	 */
	@Override
	public Future<Community> merge(Community source, String codePrefix, Vertx vertx) {

		if (source == null) {

			return Future.succeededFuture(this);

		} else {
			final Promise<Community> promise = Promise.promise();
			Future<Community> future = promise.future();

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

			future = future.compose(Merges.validateMerged(codePrefix, vertx));

			promise.complete(merged);
			// When merged set the fixed field values
			future = future.map(mergedValidatedModel -> {

				mergedValidatedModel.id = this.id;
				mergedValidatedModel._creationTs = this._creationTs;
				mergedValidatedModel._lastUpdateTs = this._lastUpdateTs;
				return mergedValidatedModel;
			});

			return future;

		}
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Future<Void> validate(String codePrefix, Vertx vertx) {

		final Promise<Void> promise = Promise.promise();
		final Future<Void> future = promise.future();
		try {

			this.id = Validations.validateNullableStringField(codePrefix, "id", 255, this.id);
			if (this.id != null) {

				throw new ValidationErrorException(codePrefix + ".id",
						"You can not specify the identifier of the community to create");

			}
			this.name = Validations.validateNullableStringField(codePrefix, "name", 255, this.name);
			this.description = Validations.validateNullableStringField(codePrefix, "description", 255, this.description);
			this.keywords = Validations.validateNullableListStringField(codePrefix, "keywords", 255, this.keywords);
			this.avatar = Validations.validateNullableURLField(codePrefix, "avatar", this.avatar);

			promise.complete();

		} catch (final ValidationErrorException validationError) {

			promise.fail(validationError);
		}

		return future;
	}
}
