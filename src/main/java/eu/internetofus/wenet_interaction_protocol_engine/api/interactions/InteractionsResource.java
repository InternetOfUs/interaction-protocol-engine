/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.interactions;

import eu.internetofus.common.components.interaction_protocol_engine.Interaction;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.vertx.ServiceRequests;
import eu.internetofus.common.vertx.ServiceResponseHandlers;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.InteractionsRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import javax.ws.rs.core.Response.Status;
import org.tinylog.Logger;

/**
 * Resource to provide the stats about the user interactions.
 *
 * @see Interactions
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class InteractionsResource implements Interactions {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * Create a new instance to provide the services of the {@link Interactions}.
   *
   * @param vertx with the event bus to use.
   */
  public InteractionsResource(final Vertx vertx) {

    this.vertx = vertx;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void getInteractionsPage(final String appId, final String communityId, final String taskTypeId,
      final String taskId, final String senderId, final String receiverId, final Boolean hasTransaction,
      final String transactionLabel, final Long transactionFrom, final Long transactionTo, final Boolean hasMessage,
      final String messageLabel, final Long messageFrom, final Long messageTo, final String orderValue,
      final int offset, final int limit, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var query = InteractionsRepository.createInteractionsPageQuery(appId, communityId, taskTypeId, taskId,
        senderId, receiverId, hasTransaction, transactionLabel, transactionFrom, transactionTo, hasMessage,
        messageLabel, messageFrom, messageTo);
    final var order = ServiceRequests.extractQueryArray(orderValue);
    InteractionsRepository.createInteractionsPageSort(order)
        .compose(
            sort -> InteractionsRepository.createProxy(this.vertx).retrieveInteractionsPage(query, sort, offset, limit))
        .onComplete(retrieve -> {

          if (retrieve.failed()) {

            final var cause = retrieve.cause();
            Logger.debug(cause, "GET /interactions with {} => Retrieve error", query);
            ServiceResponseHandlers.responseFailedWith(resultHandler, Status.BAD_REQUEST, cause);

          } else {

            final var interactionsPage = retrieve.result();
            Logger.debug("GET /interactions with {} => {}.", query, interactionsPage);
            ServiceResponseHandlers.responseOk(resultHandler, interactionsPage);
          }

        });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deleteInteractions(final String appId, final String communityId, final String taskTypeId,
      final String taskId, final String senderId, final String receiverId, final Boolean hasTransaction,
      final String transactionLabel, final Long transactionFrom, final Long transactionTo, final Boolean hasMessage,
      final String messageLabel, final Long messageFrom, final Long messageTo, final ServiceRequest context,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var query = InteractionsRepository.createInteractionsPageQuery(appId, communityId, taskTypeId, taskId,
        senderId, receiverId, hasTransaction, transactionLabel, transactionFrom, transactionTo, hasMessage,
        messageLabel, messageFrom, messageTo);
    InteractionsRepository.createProxy(this.vertx).deleteInteractions(query).onComplete(deleted -> {

      if (deleted.failed()) {

        final var cause = deleted.cause();
        Logger.debug(cause, "DELETE /interactions with {} => Retrieve error", query);
        ServiceResponseHandlers.responseFailedWith(resultHandler, Status.NOT_FOUND, cause);

      } else {

        Logger.debug("DELETE /interactions with {} => Success", query);
        ServiceResponseHandlers.responseOk(resultHandler);
      }

    });

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void addInteraction(final JsonObject body, final ServiceRequest request,
      final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    Model.fromFutureJsonObject(Future.succeededFuture(body), Interaction.class)
        .compose(interaction -> InteractionsRepository.createProxy(this.vertx).store(interaction))
        .onComplete(stored -> {

          if (stored.failed()) {

            final var cause = stored.cause();
            Logger.debug(cause, "POST /interactions with {} => Retrieve error", body);
            ServiceResponseHandlers.responseWithErrorMessage(resultHandler, Status.BAD_REQUEST, "bad_interaction",
                "The JSON does not represents a valid interaction, because "
                    + cause.getMessage().replaceAll("\\([^\\(]*\\w(\\.\\w)*[^\\(]*\\)", ""));

          } else {

            final var interaction = stored.result();
            Logger.debug("POST /interactions with {} => Success", body);
            ServiceResponseHandlers.responseWith(resultHandler, Status.CREATED, interaction);

          }

        });

  }

}
