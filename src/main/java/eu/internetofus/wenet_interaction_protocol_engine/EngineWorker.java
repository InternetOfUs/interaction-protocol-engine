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
import eu.internetofus.common.api.models.wenet.AppTextualMessage;
import eu.internetofus.common.api.models.wenet.InteractionProtocolMessage;
import eu.internetofus.common.api.models.wenet.SocialNetworkRelationship;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Handler;
import io.vertx.core.Promise;
import io.vertx.core.eventbus.Message;
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
public class EngineWorker extends AbstractVerticle implements Handler<Message<InteractionProtocolMessage>> {

	/**
	 * The address used to send messages to the worker.
	 */
	public static final String ADDRESSS = "eu.internetofus.wenet_interaction_protocol_engine.worker";

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void start(Promise<Void> startPromise) throws Exception {

		this.vertx.eventBus().consumer(ADDRESSS, this);
		startPromise.complete();

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void handle(Message<InteractionProtocolMessage> event) {

		final InteractionProtocolMessage message = event.body();
		EngineEnvironment.create(this.vertx, message).onComplete(creation -> {

			try {

				final EngineEnvironment env = creation.result();
				final WebClientOptions options = new WebClientOptions();
				final WebClient client = WebClient.create(this.vertx, options);
				final AppTextualMessage textualMessage = new AppTextualMessage();
				textualMessage.title = "Dummy protocol";
				textualMessage.text = String.valueOf(message.content);
				if (env.task.requesterId != env.sender.id) {

					textualMessage.recipientId = env.task.requesterId;
					final JsonObject body = textualMessage.toJsonObject();
					client.postAbs(env.app.messageCallbackUrl).sendJsonObject(body, send -> {
						if (send.failed()) {

							Logger.error(send.cause(), "Can not notify the requester about {}.", message);

						} else {

							Logger.debug("Sent {} to {}", body, env.app.messageCallbackUrl);
						}
					});

				} else {
					for (final SocialNetworkRelationship relation : env.sender.relationships) {

						textualMessage.recipientId = relation.userId;
						final JsonObject body = textualMessage.toJsonObject();
						client.postAbs(env.app.messageCallbackUrl).sendJsonObject(body, send -> {
							if (send.failed()) {

								Logger.error(send.cause(), "Can not notify to {} about {}.", relation.userId, message);

							} else {

								Logger.debug("Sent {} to {}", body, env.app.messageCallbackUrl);
							}
						});

					}
				}

			} catch (final Throwable throwable) {

				Logger.error(throwable, "Can not process {}", message);
			}
		});

	}

}
