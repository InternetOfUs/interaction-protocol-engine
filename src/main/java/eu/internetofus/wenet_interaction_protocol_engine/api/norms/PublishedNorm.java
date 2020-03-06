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

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import eu.internetofus.wenet_interaction_protocol_engine.ValidationErrorException;
import eu.internetofus.wenet_interaction_protocol_engine.Validations;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;

/**
 * A norm that has been published.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "A norm that is published to share with all the WeNet users.")
public class PublishedNorm extends Model {

	/**
	 * The identifier of the published norm.
	 */
	@Schema(description = "The identifier of the published norm.", example = "bf274393-1e7b-4d40-a897-88cb96277edd")
	public String _id;

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
			ref = "https://bitbucket.org/wenet/wenet-components-documentation/raw/5c0512480f89ae267d6fc0dcf42db0f3a50d01e8/sources/wenet-models.yaml#/components/schemas/Norm")
	public Norm norm;

	/**
	 * Check that the model is valid.
	 *
	 * @param codePrefix        the prefix of the code to use for the error message.
	 * @param profileManager    service to manage the profile manager.
	 * @param validationHandler handler to inform of the validation process.
	 *
	 */
	public void validate(String codePrefix, WeNetProfileManagerService profileManager,
			Handler<AsyncResult<Void>> validationHandler) {

		if (this.norm == null) {

			validationHandler.handle(Future
					.failedFuture(new ValidationErrorException(codePrefix + ".norm", "It is necessary a norm to publish.")));

		} else {
			try {

				this._id = Validations.validateNullableStringField(codePrefix, "id", 255, this._id);
				if (this._id != null) {

					throw new ValidationErrorException(codePrefix + "._id",
							"You can not specify the identifier of the published norm");

				}
				this.name = Validations.validateNullableStringField(codePrefix, "name", 255, this.name);
				this.description = Validations.validateNullableStringField(codePrefix, "description", 255, this.description);
				this.keywords = Validations.validateNullableListStringField(codePrefix, "keywords", 255, this.keywords);
				this.norm.validate(codePrefix + ".norm");

				if (this.publisherId == null) {

					validationHandler.handle(Future.succeededFuture());

				} else {

					profileManager.retrieveProfile(this.publisherId, retrieve -> {

						if (retrieve.failed()) {

							validationHandler.handle(Future.failedFuture(new ValidationErrorException(codePrefix + ".publisherId",
									"The published identifier is not valid, because it is not an active user profile.")));

						} else {

							validationHandler.handle(Future.succeededFuture());
						}
					});
				}

			} catch (final ValidationErrorException error) {

				validationHandler.handle(Future.failedFuture(error));
			}

		}
	}

	/**
	 * Merge this model with another.
	 *
	 * @param codePrefix     the prefix of the code to use for the error message.
	 * @param source         model to merge
	 * @param profileManager service to manage the profile manager.
	 * @param mergeHandler   handler to inform of the merged model.
	 *
	 */
	public void merge(String codePrefix, PublishedNorm source, WeNetProfileManagerService profileManager,
			Handler<AsyncResult<PublishedNorm>> mergeHandler) {

		if (source == null) {

			mergeHandler.handle(Future.succeededFuture(this));

		} else {

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

			if (this.norm == null) {

				merged.norm = source.norm;

			} else {

				merged.norm = new Norm();
			}

			merged.validate(codePrefix, profileManager, validation -> {

				if (validation.failed()) {

					mergeHandler.handle(Future.failedFuture(validation.cause()));

				} else {

					try {

						if (this.norm != null) {

							merged.norm = this.norm.merge(source.norm, codePrefix + ".norm");
						}

						merged._id = this._id;
						merged.publishTime = this.publishTime;
						mergeHandler.handle(Future.succeededFuture(merged));

					} catch (final ValidationErrorException error) {

						mergeHandler.handle(Future.failedFuture(error));
					}

				}

			});
		}

	}

}
