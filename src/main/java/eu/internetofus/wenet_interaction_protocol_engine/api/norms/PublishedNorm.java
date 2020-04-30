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

import java.util.List;

import eu.internetofus.common.api.models.Mergeable;
import eu.internetofus.common.api.models.Merges;
import eu.internetofus.common.api.models.Model;
import eu.internetofus.common.api.models.Validable;
import eu.internetofus.common.api.models.ValidationErrorException;
import eu.internetofus.common.api.models.Validations;
import eu.internetofus.common.api.models.wenet.Norm;
import eu.internetofus.common.services.WeNetProfileManagerService;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;

/**
 * A norm that has been published.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "A norm that is published to share with all the WeNet users.")
public class PublishedNorm extends Model implements Validable, Mergeable<PublishedNorm> {

	/**
	 * The identifier of the published norm.
	 */
	@Schema(description = "The identifier of the published norm.", example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String id;

	/**
	 * The name of the published norm.
	 */
	@Schema(description = "The name of the published norm.", example = "WeNet")
	public String name;

	/**
	 * The description of the published norm.
	 */
	@Schema(
			description = "The description of the published norm.",
			example = "A norm of users that provide or require help")
	public String description;

	/**
	 * The description of the published norm.
	 */
	@ArraySchema(
			schema = @Schema(implementation = String.class),
			arraySchema = @Schema(
					description = "The keywords of the published  norm",
					example = "[\"social interaction\",\"ethics\",\"diversity\"]"))
	public List<String> keywords;

	/**
	 * The identifier of the user that has published the norm.
	 */
	@Schema(
			description = "The identifier of the user that has published the norm.",
			example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String publisherId;

	/**
	 * The difference, measured in milliseconds, between the time when the norm was
	 * published and midnight, January 1, 1970 UTC.
	 */
	@Schema(
			description = "The difference, measured in seconds, between the time when the norm was published and midnight, January 1, 1970 UTC.",
			example = "1571413067381")
	public long publishTime;

	/**
	 * The norm.
	 */
	@Schema(
			description = "The published norm.",
			ref = "https://bitbucket.org/wenet/wenet-components-documentation/raw/master/sources/wenet-models-openapi.yaml#/components/schemas/Norm")
	public Norm norm;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Future<Void> validate(String codePrefix, Vertx vertx) {

		final Promise<Void> promise = Promise.promise();
		Future<Void> future = promise.future();
		try {

			this.id = Validations.validateNullableStringField(codePrefix, "id", 255, this.id);
			if (this.id != null) {

				future = future.compose(mapper -> {

					final Promise<Void> verifyNotRepeatedIdPromise = Promise.promise();
					NormsRepository.createProxy(vertx).searchPublishedNorm(this.id, search -> {

						if (search.failed()) {

							verifyNotRepeatedIdPromise.complete();

						} else {

							verifyNotRepeatedIdPromise.fail(new ValidationErrorException(codePrefix + ".id",
									"The '" + this.id + "' is already used by a published norm."));
						}
					});
					return verifyNotRepeatedIdPromise.future();
				});
			}

			this.name = Validations.validateNullableStringField(codePrefix, "name", 255, this.name);
			this.description = Validations.validateNullableStringField(codePrefix, "description", 1023, this.description);
			this.keywords = Validations.validateNullableListStringField(codePrefix, "keywords", 255, this.keywords);
			this.publisherId = Validations.validateNullableStringField(codePrefix, "publisherId", 255, this.publisherId);
			if (this.publisherId != null) {

				future = future.compose(mapper -> {

					final Promise<Void> verifyPublishedExistPromise = Promise.promise();
					WeNetProfileManagerService.createProxy(vertx).retrieveProfile(this.publisherId, profile -> {

						if (!profile.failed()) {

							verifyPublishedExistPromise.fail(new ValidationErrorException(codePrefix + "publisherId",
									"The '" + this.publisherId + "' is not defined as an user."));

						} else {
							verifyPublishedExistPromise.complete();
						}
					});
					return verifyPublishedExistPromise.future();
				});
			}

			if (this.norm == null) {

				promise.fail(new ValidationErrorException(codePrefix + ".norm", "you must specify a norm."));

			} else {

				future = future.compose(map -> this.norm.validate(codePrefix + ".norm", vertx));
				promise.complete();
			}

		} catch (final ValidationErrorException validationError) {

			promise.fail(validationError);
		}

		return future;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Future<PublishedNorm> merge(PublishedNorm source, String codePrefix, Vertx vertx) {

		final Promise<PublishedNorm> promise = Promise.promise();
		Future<PublishedNorm> future = promise.future();
		if (source != null) {

			final PublishedNorm merged = new PublishedNorm();
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
			merged.publisherId = source.publisherId;
			if (merged.publisherId == null) {

				merged.publisherId = this.publisherId;
			}

			merged.norm = source.norm;
			if (merged.norm == null) {

				merged.norm = new Norm();
			}

			future = future.compose(Merges.validateMerged(codePrefix, vertx));
			future = future.compose(Merges.mergeField(this.norm, source.norm, codePrefix + ".norm", vertx,
					(model, mergedNorm) -> model.norm = mergedNorm));

			promise.complete(merged);
			future = future.map(mergedValidatedModel -> {

				mergedValidatedModel.id = this.id;
				return mergedValidatedModel;
			});

		} else {

			promise.complete(this);
		}
		return future;
	}

}
