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

package eu.internetofus.wenet_interaction_protocol_engine;

import org.tinylog.Logger;

import eu.internetofus.common.Worker;
import eu.internetofus.common.api.models.Model;
import eu.internetofus.common.api.models.wenet.AppTextualMessage;
import eu.internetofus.common.api.models.wenet.InteractionProtocolMessage;
import eu.internetofus.common.services.WeNetServiceApiService;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
import io.vertx.core.eventbus.MessageConsumer;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;

/**
 * The worker verticle that is used to process the messages for an interaction
 * protocol.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Worker
public class EngineWorker extends AbstractVerticle implements Handler<Message<JsonObject>> {

	/**
	 * The address used to send messages to the worker.
	 */
	public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.worker";

	/**
	 * The component that will consume the messages.
	 */
	protected MessageConsumer<JsonObject> consumer;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		this.consumer = this.vertx.eventBus().consumer(ADDRESSS, this);
		this.consumer.completionHandler(completion -> {

			if (completion.failed()) {

				startPromise.fail(completion.cause());

			} else {

				startPromise.complete();
			}

		});

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void handle(Message<JsonObject> event) {

		try {

			final JsonObject body = event.body();
			final InteractionProtocolMessage message = Model.fromJsonObject(body, InteractionProtocolMessage.class);
			if (message == null) {

				Logger.error("Can not process the event {}, because does not contains a valid InteractionProtocolMessage.",
						event);

			} else {

				this.handle(message);
			}

		} catch (final Throwable throwable) {

			Logger.error(throwable, "Can not process the event {}", event);
		}

	}

	/**
	 * Called when have a message to process.
	 *
	 * @param message to process.
	 */
	protected void handle(InteractionProtocolMessage message) {

		EngineEnvironment.create(this.vertx, message).onComplete(creation -> {

			WeNetServiceApiService.createProxy(this.vertx).retrieveJsonArrayAppUserIds(message.appId, retrieve -> {

				if (retrieve.failed()) {

					Logger.debug("No found users from the APP");

				} else {

					try {

						final JsonArray users = retrieve.result();
						final EngineEnvironment env = creation.result();
						final WebClientOptions options = new WebClientOptions();
						final WebClient client = WebClient.create(this.vertx, options);
						final AppTextualMessage textualMessage = new AppTextualMessage();
						textualMessage.title = "Dummy protocol";
						textualMessage.text = String.valueOf(message.content);
						for (int i = 0; i < users.size(); i++) {

							final String userId = users.getString(i);
							if (message.senderId == null || !message.senderId.equals(userId)) {

								textualMessage.recipientId = userId;
								final JsonObject body = textualMessage.toJsonObject();
								client.postAbs(env.app.messageCallbackUrl).sendJsonObject(body, send -> {

									if (send.failed()) {

										Logger.error(send.cause(), "Can not notify to {} about {}.", userId, message);

									} else {

										Logger.debug("Sent {} to {}", body, env.app.messageCallbackUrl);
									}

								});
							}

						}

					} catch (final Throwable throwable) {

						Logger.error(throwable, "Can not process {}", message);
					}

				}

			});
		});

	}

}
