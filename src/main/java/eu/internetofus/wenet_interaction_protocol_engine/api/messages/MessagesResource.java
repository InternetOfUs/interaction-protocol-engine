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

package eu.internetofus.wenet_interaction_protocol_engine.api.messages;

import javax.ws.rs.core.Response.Status;

import org.tinylog.Logger;

import eu.internetofus.common.api.OperationReponseHandlers;
import eu.internetofus.common.api.models.Model;
import eu.internetofus.common.api.models.wenet.InteractionProtocolMessage;
import eu.internetofus.wenet_interaction_protocol_engine.EngineWorker;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.OperationRequest;
import io.vertx.ext.web.api.OperationResponse;

/**
 * Implementation of the {@link Messages} services.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class MessagesResource implements Messages {

	/**
	 * The event bus that is using.
	 */
	protected Vertx vertx;

	/**
	 * Create an empty resource. This is only used for unit tests.
	 */
	protected MessagesResource() {

	}

	/**
	 * Create a new instance to provide the services of the {@link Messages}.
	 *
	 * @param vertx with the event bus to use.
	 */
	public MessagesResource(Vertx vertx) {

		this.vertx = vertx;

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void sendMessage(JsonObject body, OperationRequest context,
			Handler<AsyncResult<OperationResponse>> resultHandler) {

		final InteractionProtocolMessage message = Model.fromJsonObject(body, InteractionProtocolMessage.class);
		if (message == null) {

			Logger.debug("The {} is not a valid Message.", body);
			OperationReponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_message",
					"The message is not right.");

		} else {

			message.validate("bad_message", this.vertx).onComplete(validation -> {

				if (validation.failed()) {

					final Throwable cause = validation.cause();
					Logger.debug(cause, "The {} is not valid.", message);
					OperationReponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

				} else {

					OperationReponseHandlers.responseOk(resultHandler, message);
					try {

						this.vertx.eventBus().publish(EngineWorker.ADDRESSS, message.toJsonObject());
						Logger.debug("Valid {}, so sent it to the engine.", message);

					} catch (final Throwable throwable) {

						Logger.error(throwable, "Can not send {} to the engine.", message);

					}

				}

			});

		}
	}

}
