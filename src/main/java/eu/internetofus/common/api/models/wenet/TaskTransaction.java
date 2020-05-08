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

package eu.internetofus.common.api.models.wenet;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import eu.internetofus.common.api.models.JsonObjectDeserializer;
import eu.internetofus.common.api.models.Model;
import eu.internetofus.common.api.models.Validable;
import eu.internetofus.common.api.models.ValidationErrorException;
import eu.internetofus.common.api.models.Validations;
import io.swagger.v3.oas.annotations.media.Schema;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;

/**
 * Describe a transition to do over a task.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(hidden = true, name = "TaskTransaction", description = "Describe a transition to do over a task.")
public class TaskTransaction extends Model implements Validable {

	/**
	 * The identifier of the task that it refers.
	 */
	@Schema(description = "The unique identifier this transaction is associated to.", example = "b129e5509c9bb79")
	public String taskId;

	/**
	 * The identifier of the task type.
	 */
	@Schema(description = "The label associated to the transaction type.", example = "acceptVolunteer")
	public String label;

	/**
	 * The attributes set to the transaction.
	 */
	@Schema(type = "object", description = "The attributes that are set in the associated transaction type.")
	@JsonDeserialize(using = JsonObjectDeserializer.class)
	public JsonObject attributes;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Future<Void> validate(String codePrefix, Vertx vertx) {

		final Promise<Void> promise = Promise.promise();
		final Future<Void> future = promise.future();
		try {

			this.taskId = Validations.validateStringField(codePrefix, "taskId", 255, this.taskId);
			this.label = Validations.validateStringField(codePrefix, "label", 255, this.label);
			// TODO verify the attributes are valid for the task transaction type
			promise.complete();

		} catch (final ValidationErrorException validationError) {

			promise.fail(validationError);
		}

		return future;
	}

}
