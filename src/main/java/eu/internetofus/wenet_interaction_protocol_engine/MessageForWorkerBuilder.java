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

package eu.internetofus.wenet_interaction_protocol_engine;

import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolAddress.Component;
import eu.internetofus.common.components.interaction_protocol_engine.ProtocolMessage;
import eu.internetofus.common.components.models.Incentive;
import eu.internetofus.common.components.models.TaskTransaction;
import io.vertx.core.json.JsonObject;

/**
 * Component used to create the messages that can be send to a worker.
 *
 * @see EngineWorker
 *
 * @author UDT-IA, IIIA-CSIC
 */
public interface MessageForWorkerBuilder {

  /**
   * The types of messages that the worker can receive.
   */
  public enum Type {
    /**
     * When the message inform that a task is created.
     */
    CREATED_TASK,

    /**
     * When the message inform about a task transaction that has to be done.
     */
    DO_TASK_TRANSACTION,

    /**
     * When the message inform about an incentive to inform to the user.
     */
    SEND_INCENTIVE,

    /**
     * When the message refers to the interaction protocol.
     */
    PROTOCOL_MESSAGE;

  }

  /**
   * Create the message for the worker with the protocol message to process.
   *
   * @param message  to send.
   * @param protocol information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessage(final ProtocolMessage message, final ProtocolData protocol) {

    return new JsonObject().put("type", Type.PROTOCOL_MESSAGE.name()).put("message", message.toJsonObject())
        .put("protocol", protocol.toJsonObject());

  }

  /**
   * Create the protocol message for a protocol.
   *
   * @param protocol to create the message.
   *
   * @return the message associated to the protocol.
   */
  public static ProtocolMessage createProtocolMessage(final ProtocolData protocol) {

    final var message = new ProtocolMessage();
    if (protocol.task != null) {

      message.appId = protocol.task.appId;
      message.communityId = protocol.task.communityId;
      message.taskId = protocol.task.id;

    } else if (protocol.community != null) {

      message.communityId = protocol.community.id;
      message.appId = protocol.community.appId;
    }
    message.sender = new ProtocolAddress();
    message.receiver = new ProtocolAddress();
    message.receiver.component = Component.INTERACTION_PROTOCOL_ENGINE;
    if (protocol.profile != null) {

      message.receiver.userId = protocol.profile.id;
    }
    return message;

  }

  /**
   * Create the message for the worker with the protocol message to process an
   * incentive to send.
   *
   * @param incentive to send.
   * @param protocol  information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForSendIncentive(final Incentive incentive,
      final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.appId = incentive.AppID;
    message.sender.component = Component.INCENTIVE_SERVER;
    message.receiver.userId = incentive.UserId;
    message.particle = "sendIncentive";
    message.content = incentive.toJsonObject();
    return buildProtocolMessage(message, protocol);

  }

  /**
   * Create the message for the worker with the protocol message to process a
   * created task event.
   *
   * @param protocol information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForCreatedTask(final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.sender.component = Component.TASK_MANAGER;
    message.sender.userId = message.receiver.userId = protocol.task.requesterId;
    message.particle = "createdTask";
    return buildProtocolMessage(message, protocol);

  }

  /**
   * Create the message for the worker with the protocol message to process a
   * transaction to do.
   *
   * @param transaction to do.
   * @param protocol    information of the protocol to use.
   *
   * @return the message to send to the worker.
   */
  public static JsonObject buildProtocolMessageForDoTaskTransaction(final TaskTransaction transaction,
      final ProtocolData protocol) {

    final var message = createProtocolMessage(protocol);
    message.sender.component = Component.USER_APP;
    message.sender.userId = message.receiver.userId = transaction.actioneerId;
    message.particle = "doTaskTransaction";
    message.content = transaction.toJsonObject();
    return buildProtocolMessage(message, protocol);

  }

}
