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

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.interaction_protocol_engine.InteractionProtocolMessage;
import eu.internetofus.common.components.service.App;
import eu.internetofus.common.components.service.MessageFromUserNotification;
import eu.internetofus.common.components.service.TaskConcludedNotification;
import eu.internetofus.common.components.service.TaskProposalNotification;
import eu.internetofus.common.components.service.TaskSelectionNotification;
import eu.internetofus.common.components.service.TaskVolunteerNotification;
import eu.internetofus.common.components.service.WeNetServiceApiService;
import eu.internetofus.common.vertx.Worker;
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
  public void start(final Promise<Void> startPromise) throws Exception {

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
  public void handle(final Message<JsonObject> event) {

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
  protected void handle(final InteractionProtocolMessage message) {

    EngineEnvironment.create(this.vertx, message).onComplete(creation -> {

      try {

        final EngineEnvironment env = creation.result();
        if (message.content instanceof JsonObject) {

          final JsonObject content = (JsonObject) message.content;
          final String action = content.getString("action");
          final WebClientOptions options = new WebClientOptions();
          final WebClient client = WebClient.create(this.vertx, options);
          if ("TaskCreation".equalsIgnoreCase(action)) {

            final TaskProposalNotification notification = new TaskProposalNotification();
            notification.taskId = env.task.id;
            notification.title = "Can you help?";
            notification.text = env.task.goal.name;
            notification.title = "Can you help?";
            this.sendToAllAppUsers(env.app, client, notification, env.task.requesterId);



          } else if ("volunteerForTask".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId",
                "0");
            if (env.task.deadlineTs != null && env.task.deadlineTs > TimeManager.now()) {

              final TaskVolunteerNotification notification = new TaskVolunteerNotification();
              notification.recipientId = env.task.requesterId;
              notification.taskId = env.task.id;
              notification.title = "Found volunteer";
              notification.text = "An user want to help you.";
              notification.volunteerId = volunteerId;
              this.sendTo(env.app, client, notification);

            } else {
              // too late
              final MessageFromUserNotification notification = new MessageFromUserNotification();
              notification.recipientId = volunteerId;
              notification.taskId = env.task.id;
              notification.title = "Deadline reached";
              notification.text = "It's too late to volunteer.";
              notification.senderId = env.task.requesterId;
              this.sendTo(env.app, client, notification);

            }

          } else if ("refuseTask".equalsIgnoreCase(action)) {
            // nothing to do
            Logger.debug("Ignored {}, because any action is necessary", message);

          } else if ("acceptVolunteer".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId",
                "0");
            final TaskSelectionNotification notification = new TaskSelectionNotification();
            notification.recipientId = volunteerId;
            notification.taskId = env.task.id;
            notification.title = "Help not needed";
            notification.text = "You has been selected to do r help is not necessary to do the task.";
            notification.outcome = TaskSelectionNotification.Outcome.accepted;
            this.sendTo(env.app, client, notification);

          } else if ("refuseVolunteer".equalsIgnoreCase(action)) {

            final String volunteerId = content.getJsonObject("attributes", new JsonObject()).getString("volunteerId",
                "0");
            final TaskSelectionNotification notification = new TaskSelectionNotification();
            notification.recipientId = volunteerId;
            notification.taskId = env.task.id;
            notification.title = "Help not needed";
            notification.text = "Your help is not necessary to do the task.";
            notification.outcome = TaskSelectionNotification.Outcome.refused;
            this.sendTo(env.app, client, notification);

          } else if ("taskCompleted".equalsIgnoreCase(action)) {

            final String outcome = content.getJsonObject("attributes", new JsonObject()).getString("outcome",
                "cancelled");
            final TaskConcludedNotification notification = new TaskConcludedNotification();
            notification.taskId = env.task.id;
            notification.title = "Help not needed";
            notification.text = "Your help is not necessary to do the task.";
            notification.outcome = TaskConcludedNotification.Outcome.valueOf(outcome);
            this.sendToAllAppUsers(env.app, client, notification, env.task.requesterId);

          } else {

            Logger.error("Unexpected action {}", message);
          }

        } else {

          Logger.error("Unexpected {}", message);
        }

      } catch (final Throwable throwable) {

        Logger.error(throwable, "Can not process {}", message);
      }

    });

  }

  /**
   * Called when want to notify all users about a notification.
   *
   * @param app          application to notify all the users.
   * @param client       to use.
   * @param notification to send to the users.
   * @param senderId     identifier of the user that send the notification.
   */
  private void sendToAllAppUsers(final App app, final WebClient client,
      final eu.internetofus.common.components.service.Message notification, final String senderId) {

    WeNetServiceApiService.createProxy(this.vertx).retrieveJsonArrayAppUserIds(app.appId, retrieve -> {

      if (retrieve.failed()) {

        Logger.debug("No found users from the APP to send {}", notification);

      } else {

        final JsonArray users = retrieve.result();
        for (int i = 0; i < users.size(); i++) {

          final String userId = users.getString(i);
          if (!senderId.equals(userId)) {

            notification.recipientId = userId;
            this.sendTo(app, client, notification);
          }

        }

      }
    });

  }

  /**
   * Called when want to send a notification to an application.
   *
   * @param app          application to notify.
   * @param client       to use.
   * @param notification to send to the application.
   */
  private void sendTo(final App app, final WebClient client, final eu.internetofus.common.components.service.Message notification) {

    final JsonObject body = notification.toJsonObject();
    client.postAbs(app.messageCallbackUrl).sendJsonObject(body, send -> {

      if (send.failed()) {

        Logger.error(send.cause(), "Can not notify about  {} to {}.", body, app);

      } else {

        Logger.debug("Sent {}  to {}", body, app);
      }

    });

  }

}
